//
//Copyright (C) 2007 United States Government as represented by the
// Administrator of the National Aeronautics and Space Administration
// (NASA).  All Rights Reserved.
//
// This software is distributed under the NASA Open Source Agreement
// (NOSA), version 1.3.  The NOSA has been approved by the Open Source
// Initiative.  See the file NOSA-1.3-JPF at the top of the distribution
// directory tree for the complete NOSA document.
//
// THE SUBJECT SOFTWARE IS PROVIDED "AS IS" WITHOUT ANY WARRANTY OF ANY
// KIND, EITHER EXPRESSED, IMPLIED, OR STATUTORY, INCLUDING, BUT NOT
// LIMITED TO, ANY WARRANTY THAT THE SUBJECT SOFTWARE WILL CONFORM TO
// SPECIFICATIONS, ANY IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR
// A PARTICULAR PURPOSE, OR FREEDOM FROM INFRINGEMENT, ANY WARRANTY THAT
// THE SUBJECT SOFTWARE WILL BE ERROR FREE, OR ANY WARRANTY THAT
// DOCUMENTATION, IF PROVIDED, WILL CONFORM TO THE SUBJECT SOFTWARE.

package gov.nasa.jpf.symbc.sequences;

// does not work well for static methods:summary not printed for errors

import gov.nasa.jpf.Config;
import gov.nasa.jpf.JPF;
import gov.nasa.jpf.Property;
import gov.nasa.jpf.PropertyListenerAdapter;
import gov.nasa.jpf.jvm.bytecode.JVMInvokeInstruction;
import gov.nasa.jpf.report.ConsolePublisher;
import gov.nasa.jpf.report.Publisher;
import gov.nasa.jpf.report.PublisherExtension;
import gov.nasa.jpf.search.Search;
import gov.nasa.jpf.symbc.SymbolicInstructionFactory;
import gov.nasa.jpf.symbc.SymbolicListenersUtils;
import gov.nasa.jpf.symbc.bytecode.BytecodeUtils;
import gov.nasa.jpf.symbc.bytecode.INVOKESTATIC;
import gov.nasa.jpf.symbc.concolic.PCAnalyzer;
import gov.nasa.jpf.symbc.numeric.IntegerExpression;
import gov.nasa.jpf.symbc.numeric.PCChoiceGenerator;
import gov.nasa.jpf.symbc.numeric.PathCondition;
import gov.nasa.jpf.symbc.numeric.RealExpression;
import gov.nasa.jpf.symbc.numeric.SymbolicConstraintsGeneral;
import gov.nasa.jpf.symbc.string.StringSymbolic;
import gov.nasa.jpf.vm.*;

import java.io.PrintWriter;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.Set;
import java.util.Vector;

/**
 * @author Mithun Acharya
 *         with inputs from Corina.
 *         <p>
 *         Note that all the methods of interest should be specified in +symbolic.method option.
 *         if a method is not specified in +symbolic.method it will not be printed.
 *         even if the method, foo(int i) is invoked concretely always, we should have
 *         foo(con) in +symbolic.method option
 *         <p>
 *         Algorithm (Works independent of search order):
 *         <p>
 *         When instructionExecuted->JVMInvokeInstruction, remember the executed method, symbolic attributes, etc.
 *         in a SequenceChoiceGenerator
 *         <p>
 *         The main idea is to exploit the fact that
 *         "at each state, the path from start state to the current state has a
 *         unique chain of choice generators"
 *         <p>
 *         During stateBacktracked or propertyViolated, get the chain of choice generators. In this chain look
 *         for SequenceChoiceGenerators (which hold information about symbolically executed methods).
 *         With the current path condition solution and the symbolic attributes
 *         stored in SequenceChoiceGenerators, output the concrete method sequence.
 *         <p>
 *         <p>
 *         KNOWN PROBLEMS:
 *         <p>
 *         1) For JUnit test cases, getting class name and object name is not smart.
 */
public class SymbolicSequenceListener extends PropertyListenerAdapter implements PublisherExtension
{


    // custom marker to mark error strings in method sequences
    private final static String exceptionMarker = "##EXCEPTION## ";

    // Name of the class under test
    String _className = "";

    // this set will store all the method sequences.
    // will be printed at last.
    // '_methodSequences' is a set of 'methodSequence's
    // A single 'methodSequence' is a vector of invoked 'method's along a path
    // A single invoked 'method' is represented as a String.
    Set<Vector> _methodSequences = new LinkedHashSet<Vector>();

    public SymbolicSequenceListener(Config conf, JPF jpf)
    {
        jpf.addPublisherExtension(ConsolePublisher.class, this);
    }

    @Override
    public void instructionExecuted(VM vm, ThreadInfo currentThread, Instruction nextInstruction, Instruction executedInstruction)
    {
        if (vm.getSystemState().isIgnored()) {
            return;
        }
        if (!executedInstruction.isCompleted(currentThread)) {
            return;
        }
        if (!( executedInstruction instanceof JVMInvokeInstruction )) {
            return;
        }

        JVMInvokeInstruction invokeInstruction = (JVMInvokeInstruction)executedInstruction;
        int numberOfArgs = invokeInstruction.getArgumentValues(currentThread).length;
        MethodInfo methodInfo = invokeInstruction.getInvokedMethod();

        StackFrame stackFrame = currentThread.getTopFrame();
        if (!methodInfo.equals(stackFrame.getMethodInfo())) {
            return;
        }

        Config conf = vm.getConfig();
        if (( BytecodeUtils.isMethodSymbolic(conf, methodInfo.getFullName(), numberOfArgs, null) )) {

            // FIXME: get the object name?
            // VirtualInvocation virtualInvocation = (VirtualInvocation)insn;
            // int ref = virtualInvocation.getThis(ti);
            // DynamicElementInfo d = ss.ks.da.get(ref);
            // right now I am just getting the class name
            _className = methodInfo.getClassName();

            // get arg values
            Object[] argValues = invokeInstruction.getArgumentValues(currentThread);

            Object[] attributes = SymbolicListenersUtils.getArgumentAttributes(methodInfo, stackFrame, numberOfArgs, invokeInstruction instanceof
                    INVOKESTATIC);

            // Create a new SequenceChoiceGenerator.
            // this is just to store the information
            // regarding the executed method.
            SequenceChoiceGenerator choiceGenerator = SequenceChoiceGenerator.newInstance(getShortMethodName(invokeInstruction), argValues,
                    attributes);
            // Does not actually make any choice
            SystemState systemState = vm.getSystemState();
            systemState.setNextChoiceGenerator(choiceGenerator);
            // nothing to do as there are no choices.
        }

        //else if (insn instanceof JVMReturnInstruction){
        // I don't think we need to do anything  here for printing method sequences...
        //}

    }

    @Override
    public void propertyViolated(Search search)
    {
        System.out.println("--------->property violated");
        VM vm = search.getVM();
        SystemState ss = vm.getSystemState();
        final PCChoiceGenerator cg = SymbolicListenersUtils.searchForPCChoiceGenerator(vm.getChoiceGenerator());
        if (cg == null) {
            return;
        }
        Property prop = search.getLastError().getProperty();
        String errAnn = "";
        if (prop instanceof NoUncaughtExceptionsProperty) {
            String exceptionClass = ( (NoUncaughtExceptionsProperty)prop ).getUncaughtExceptionInfo().getExceptionClassname();
            errAnn = "(expected = " + exceptionClass + ".class)";
        }

        String error = search.getLastError().getDetails();
        error = "\"" + error.substring(0, error.indexOf("\n")) + "...\"";

        final PathCondition pathCondition = cg.getCurrentPC();
        if (pathCondition == null) {
            return;
        }

        System.out.println("Path condition " + pathCondition.count() + " " + pathCondition.toString());

        //solve the path condition
        SymbolicListenersUtils.solve(pathCondition);

        // get the chain of choice generators.
        ChoiceGenerator<?>[] cgs = ss.getChoiceGenerators();
        Vector<String> methodSequence = getMethodSequence(cgs);
        // Now append the error String and then add methodSequence to _methodSequences
        // prefix the exception marker to distinguish this from
        // an invoked method.
        if (errAnn != "") {
            methodSequence.add(0, errAnn);
        }
        methodSequence.add(exceptionMarker + error);
        _methodSequences.add(methodSequence);


    }

    //	-------- the publisher interface
    public void publishFinished(Publisher publisher)
    {

        PrintWriter pw = publisher.getOut();
        // here just print the method sequences
        publisher.publishTopicStart("Method Sequences");
        printMethodSequences(pw);

        // print JUnit4.0 test class
        publisher.publishTopicStart("JUnit 4.0 test class");
        printJUnitTestClass(pw);

    }

    public void stateBacktracked(Search search)
    {
        VM vm = search.getVM();
        Config conf = vm.getConfig();

        Instruction insn = vm.getChoiceGenerator().getInsn();
        SystemState ss = vm.getSystemState();
        //ThreadInfo ti = vm.getChoiceGenerator().getThreadInfo();
        MethodInfo mi = insn.getMethodInfo();
        String methodName = mi.getFullName();

        int numberOfArgs = mi.getNumberOfArguments();//mi.getArgumentsSize()- 1;// corina: problem here? - 1;

        //	if (BytecodeUtils.isMethodSymbolic(conf, methodName, numberOfArgs, null)){

        ChoiceGenerator<?> cg = vm.getChoiceGenerator();

        if (!( cg instanceof PCChoiceGenerator )) {
            ChoiceGenerator<?> prev_cg = cg.getPreviousChoiceGenerator();
            while (!( ( prev_cg == null ) || ( prev_cg instanceof PCChoiceGenerator ) )) {
                prev_cg = prev_cg.getPreviousChoiceGenerator();
            }
            cg = prev_cg;
        }

        if (( cg instanceof PCChoiceGenerator ) &&
                ( (PCChoiceGenerator)cg ).getCurrentPC() != null) {

            PathCondition pc = ( (PCChoiceGenerator)cg ).getCurrentPC();
            //solve the path condition
            if (SymbolicInstructionFactory.concolicMode) { //TODO: cleaner
                SymbolicConstraintsGeneral solver = new SymbolicConstraintsGeneral();
                PCAnalyzer pa = new PCAnalyzer();
                pa.solve(pc, solver);
            } else
                pc.solve();
            // get the chain of choice generators.
            ChoiceGenerator<?>[] cgs = ss.getChoiceGenerators();
            _methodSequences.add(getMethodSequence(cgs));
        }
        //	}
    }

    /**
     * A single invoked 'method' is represented as a String.
     * information about the invoked method is got from the SequenceChoiceGenerator
     */
    private String getInvokedMethod(SequenceChoiceGenerator cg)
    {
        String invokedMethod = "";

        // get method name
        String shortName = cg.getMethodShortName();
        invokedMethod += shortName + "(";

        // get argument values
        Object[] argValues = cg.getArgValues();

        // get number of arguments
        int numberOfArgs = argValues.length;

        // get symbolic attributes
        Object[] attributes = cg.getArgAttributes();

        // get the solution
        for (int i = 0; i < numberOfArgs; i++) {
            Object attribute = attributes[i];
            if (attribute != null) { // parameter symbolic
                // here we should consider different types of symbolic arguments
                //IntegerExpression e = (IntegerExpression)attributes[i];
                Object e = attributes[i];
                String solution = "";

                if (e instanceof IntegerExpression) {
                    // trick to print bools correctly
                    if (argValues[i].toString() == "true" || argValues[i].toString() == "false") {
                        if (( (IntegerExpression)e ).solution() == 0)
                            solution = solution + "false";
                        else
                            solution = solution + "true";
                    } else
                        solution = solution + ( (IntegerExpression)e ).solution();
                } else if (e instanceof RealExpression)
                    solution = solution + ( (RealExpression)e ).solution();
                else
                    solution = solution + ( (StringSymbolic)e ).solution();
                invokedMethod += solution + ",";
            } else { // parameter concrete - for a concrete parameter, the symbolic attribute is null
                invokedMethod += argValues[i] + ",";
            }
        }

        // remove the extra comma
        if (invokedMethod.endsWith(","))
            invokedMethod = invokedMethod.substring(0, invokedMethod.length() - 1);
        invokedMethod += ")";

        return invokedMethod;
    }

    /**
     * traverses the ChoiceGenerator chain to get the method sequence
     * looks for SequenceChoiceGenerator in the chain
     * SequenceChoiceGenerators have information about the methods
     * executed and hence the method sequence can be obtained.
     * A single 'methodSequence' is a vector of invoked 'method's along a path
     * A single invoked 'method' is represented as a String.
     */
    private Vector<String> getMethodSequence(ChoiceGenerator[] cgs)
    {
        // A method sequence is a vector of strings
        Vector<String> methodSequence = new Vector<String>();
        ChoiceGenerator cg = null;
        // explore the choice generator chain - unique for a given path.
        for (int i = 0; i < cgs.length; i++) {
            cg = cgs[i];
            if (( cg instanceof SequenceChoiceGenerator )) {
                methodSequence.add(getInvokedMethod((SequenceChoiceGenerator)cg));
            }
        }
        return methodSequence;
    }

    /**
     * Returns invoked method name without signature part.
     *
     * @param invokeInstruction
     * @return
     */
    private String getShortMethodName(JVMInvokeInstruction invokeInstruction)
    {
        String shortName = invokeInstruction.getInvokedMethodName();
        if (shortName.contains("(")) {
            return shortName.substring(0, shortName.indexOf("("));
        }
        return shortName;
    }

    /**
     * @author Mithun Acharya
     * Dumb printing of JUnit 4.0 test class
     * FIXME: getting class name and object name is not smart.
     */
    private void printJUnitTestClass(PrintWriter pw)
    {
        // imports
        pw.println("import static org.junit.Assert.*;");
        pw.println("import org.junit.Before;");
        pw.println("import org.junit.Test;");

        String objectName = ( _className.toLowerCase() ).replace(".", "_");

        pw.println();
        pw.println("public class " + _className.replace(".", "_") + "Test {"); // test class
        pw.println();
        pw.println("	private " + _className + " " + objectName + ";"); // CUT object to be tested
        pw.println();
        pw.println("	@Before"); // setUp method annotation
        pw.println("	public void setUp() throws Exception {"); // setUp method
        pw.println("		" + objectName + " = new " + _className + "();"); // create object for CUT
        pw.println("	}"); // setUp method end
        // Create a test method for each sequence
        int testIndex = 0;
        Iterator<Vector> it = _methodSequences.iterator();
        while (it.hasNext()) {
            Vector<String> methodSequence = it.next();
            pw.println();
            Iterator<String> it1 = methodSequence.iterator();
            if (it1.hasNext()) {
                String errAnn = (String)( it1.next() );

                if (errAnn.contains("expected")) {
                    pw.println("	@Test" + errAnn); // Corina: added @Test annotation with exception expected
                } else {
                    pw.println("	@Test"); // @Test annotation
                    it1 = methodSequence.iterator();
                }
            } else
                it1 = methodSequence.iterator();

            //pw.println("	@Test"); // @Test annotation
            pw.println("	public void test" + testIndex + "() {"); // begin test method
            //Iterator<String> it1 = methodSequence.iterator();
            while (it1.hasNext()) {
                String invokedMethod = it1.next();
                if (invokedMethod.contains(exceptionMarker)) { // error-string. not a method
                    // add a comment about the exception
                    pw.println("		" + "//should lead to " + invokedMethod);
                } else { // normal method
                    pw.println("		" + objectName + "." + invokedMethod + ";"); // invoke a method in the sequence
                }
            }
            pw.println("	}"); // end test method
            testIndex++;
        }
        pw.println("}"); // test class end
    }

    /**
     * @author Mithun Acharya
     * <p>
     * prints the method sequences
     */
    private void printMethodSequences(PrintWriter pw)
    {
        Iterator<Vector> it = _methodSequences.iterator();
        while (it.hasNext()) {
            pw.println(it.next());
        }
    }

}
