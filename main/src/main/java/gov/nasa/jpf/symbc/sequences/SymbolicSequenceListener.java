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
import gov.nasa.jpf.symbc.SymbolicListenersUtils;
import gov.nasa.jpf.symbc.bytecode.BytecodeUtils;
import gov.nasa.jpf.symbc.bytecode.INVOKESTATIC;
import gov.nasa.jpf.symbc.numeric.PCChoiceGenerator;
import gov.nasa.jpf.symbc.numeric.PathCondition;
import gov.nasa.jpf.vm.*;

import java.io.PrintWriter;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

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

    private static final String EMPTY = "";

    // custom marker to mark error strings in method sequences
    private final static String exceptionMarker = "##EXCEPTION## ";

    // Name of the class under test
    String _className = EMPTY;

    // this set will store all the method sequences.
    // will be printed at last.
    // '_methodSequences' is a set of 'methodSequence's
    // A single 'methodSequence' is a vector of invoked 'method's along a path
    // A single invoked 'method' is represented as a String.
    Set<List<String>> _methodSequences = new LinkedHashSet<List<String>>();

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
        final PCChoiceGenerator cg = SymbolicListenersUtils.searchForPCChoiceGenerator(vm.getChoiceGenerator());
        if (cg == null) {
            return;
        }
        Property prop = search.getLastError().getProperty();
        String errAnn = EMPTY;
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
        SystemState systemState = vm.getSystemState();
        ChoiceGenerator<?>[] cgs = systemState.getChoiceGenerators();
        List<String> methodSequence = SymbolicListenersUtils.getMethodSequence(cgs);
        // Now append the error String and then add methodSequence to _methodSequences
        // prefix the exception marker to distinguish this from
        // an invoked method.
        if (!EMPTY.equals(errAnn)) {
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
        SystemState systemState = vm.getSystemState();
        //        Config conf = vm.getConfig();
        //        Instruction insn = vm.getChoiceGenerator().getInsn();
        //ThreadInfo ti = vm.getChoiceGenerator().getThreadInfo();
        //        MethodInfo mi = insn.getMethodInfo();
        //        String methodName = mi.getFullName();
        //        int numberOfArgs = mi.getNumberOfArguments();//mi.getArgumentsSize()- 1;// corina: problem here? - 1;
        //	if (BytecodeUtils.isMethodSymbolic(conf, methodName, numberOfArgs, null)){

        PCChoiceGenerator choiceGenerator = SymbolicListenersUtils.searchForPCChoiceGenerator(vm.getChoiceGenerator());
        if (choiceGenerator == null) {
            return;
        }
        PathCondition pathCondition = choiceGenerator.getCurrentPC();
        if (pathCondition != null) {
            SymbolicListenersUtils.solve(pathCondition);
            // get the chain of choice generators.
            ChoiceGenerator<?>[] cgs = systemState.getChoiceGenerators();
            _methodSequences.add(SymbolicListenersUtils.getMethodSequence(cgs));
        }
        //	}
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
        Iterator<List<String>> it = _methodSequences.iterator();
        while (it.hasNext()) {
            List<String> methodSequence = it.next();
            pw.println();
            Iterator<String> it1 = methodSequence.iterator();
            if (it1.hasNext()) {
                String errAnn = it1.next();

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
        Iterator<List<String>> it = _methodSequences.iterator();
        while (it.hasNext()) {
            pw.println(it.next());
        }
    }

}
