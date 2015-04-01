//TODO: needs to be simplified;

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

package gov.nasa.jpf.symbc;


import gov.nasa.jpf.Config;
import gov.nasa.jpf.JPF;
import gov.nasa.jpf.PropertyListenerAdapter;
import gov.nasa.jpf.jvm.bytecode.*;
import gov.nasa.jpf.report.ConsolePublisher;
import gov.nasa.jpf.report.Publisher;
import gov.nasa.jpf.report.PublisherExtension;
import gov.nasa.jpf.search.Search;
import gov.nasa.jpf.symbc.bytecode.BytecodeUtils;
import gov.nasa.jpf.symbc.bytecode.INVOKESTATIC;
import gov.nasa.jpf.symbc.concolic.PCAnalyzer;
import gov.nasa.jpf.symbc.numeric.*;
import gov.nasa.jpf.util.Pair;
import gov.nasa.jpf.vm.*;

import java.io.PrintWriter;
import java.util.Iterator;
import java.util.Map;
import java.util.StringTokenizer;
import java.util.Vector;
import java.util.concurrent.ConcurrentHashMap;

//import gov.nasa.jpf.symbc.numeric.SymbolicInteger;


public class SymbolicListener extends PropertyListenerAdapter implements PublisherExtension
{

    /* Locals to preserve the value that was held by JPF prior to changing it
     * in order to turn off state matching during symbolic execution
     * no longer necessary because we run spf stateless */


    private static final String EMPTY = "";

    private final Map<String, MethodSummary> _methodSummaries;

    private String currentMethodName = EMPTY;

    public SymbolicListener(Config conf, JPF jpf)
    {
        jpf.addPublisherExtension(ConsolePublisher.class, this);
        _methodSummaries = new ConcurrentHashMap<String, MethodSummary>();
    }

    //Writes the method summaries to a file for use in another application
    //	private void writeTable(){
    //	  try {
    //	        BufferedWriter out = new BufferedWriter(new FileWriter("outFile.txt"));
    //		    Iterator it = _methodSummaries.entrySet().iterator();
    //		    String line = "";
    //		    while (it.hasNext()){
    //		    	Map.Entry me = (Map.Entry)it.next();
    //		    	String _methodName = (String)me.getKey();
    //		    	MethodSummary ms = (MethodSummary)me.getValue();
    //		    	line = "METHOD: " + _methodName + "," +
    //		    		ms.getMethodName() + "(" + ms.getArgValues() + ")," +
    //		    		ms.getMethodName() + "(" + ms.getSymValues() + ")";
    //		    	out.write(line);
    //		    	out.newLine();
    //		    	Vector<Pair> pathConditions = ms.getPathConditions();
    //				  if (pathConditions.size() > 0){
    //					  Iterator it2 = pathConditions.iterator();
    //					  while(it2.hasNext()){
    //						  Pair pcPair = (Pair)it2.next();
    //						  String pc = (String)pcPair.a;
    //						  String errorMessage = (String)pcPair.b;
    //						  line = pc;
    //						  if (!errorMessage.equalsIgnoreCase(""))
    //							  line = line + "$" + errorMessage;
    //						  out.write(line);
    //						  out.newLine();
    //					  }
    //				  }
    //		    }
    //	        out.close();
    //	    } catch (Exception e) {
    //	    }
    //	}

    @Override
    public void instructionExecuted(VM vm, ThreadInfo currentThread, Instruction nextInstruction, Instruction executedInstruction)
    {
        if (!vm.getSystemState().isIgnored()) {
            Instruction instruction = executedInstruction;
            //	SystemState ss = vm.getSystemState();
            ThreadInfo threadInfo = currentThread;
            Config conf = vm.getConfig();
            if (instruction instanceof JVMInvokeInstruction) {
                executeInvokeInstruction((JVMInvokeInstruction)instruction, threadInfo, conf);
            } else if (instruction instanceof JVMReturnInstruction) {
                executeReturnInstruction(vm, (JVMReturnInstruction)instruction, threadInfo, conf);
            }
        }
    }

    @Override
    public void propertyViolated(Search search)
    {

        VM vm = search.getVM();
        ChoiceGenerator<?> cg = vm.getChoiceGenerator();
        cg = searchForPCChoiceGenerator(cg);
        if (( cg instanceof PCChoiceGenerator ) &&
                ( (PCChoiceGenerator)cg ).getCurrentPC() != null) {
            PathCondition pc = ( (PCChoiceGenerator)cg ).getCurrentPC();
            String error = search.getLastError().getDetails();
            error = "\"" + error.substring(0, error.indexOf("\n")) + "...\"";
            // C: not clear where result was used here -- to review
            //PathCondition result = new PathCondition();
            //IntegerExpression sym_err = new SymbolicInteger("ERROR");
            //IntegerExpression sym_value = new SymbolicInteger(error);
            //result._addDet(Comparator.EQ, sym_err, sym_value);
            //solve the path condition, then print it
            //pc.solve();
            if (SymbolicInstructionFactory.concolicMode) { //TODO: cleaner
                SymbolicConstraintsGeneral solver = new SymbolicConstraintsGeneral();
                PCAnalyzer pa = new PCAnalyzer();
                pa.solve(pc, solver);
            } else {
                pc.solve();
            }

            Pair<String, String> pcPair = new Pair<String, String>(pc.toString(), error);//(pc.toString(),error);

            //String _methodName = vm.getLastInstruction().getMethodInfo().getName();
            MethodSummary methodSummary = _methodSummaries.get(currentMethodName);
            methodSummary.addPathCondition(pcPair);
            _methodSummaries.put(currentMethodName, methodSummary);
            System.out.println("Property Violated: PC is " + pc.toString());
            System.out.println("Property Violated: result is  " + error);
            System.out.println("****************************");
        }
        //}
    }

    //	-------- the publisher interface
    @Override
    public void publishFinished(Publisher publisher)
    {
        String[] dp = SymbolicInstructionFactory.dp;
        if (dp[0].equalsIgnoreCase("no_solver") || dp[0].equalsIgnoreCase("cvc3bitvec")) {
            return;
        }
        PrintWriter pw = publisher.getOut();

        publisher.publishTopicStart("Method Summaries");
        Iterator<Map.Entry<String, MethodSummary>> it = _methodSummaries.entrySet().iterator();
        while (it.hasNext()) {
            Map.Entry<String, MethodSummary> me = it.next();
            MethodSummary methodSummary = me.getValue();
            pw.println();
            pw.println();
            pw.println(me.getKey());
            printMethodSummary(pw, methodSummary);
        }

        publisher.publishTopicStart("Method Summaries (HTML)");
        it = _methodSummaries.entrySet().iterator();
        while (it.hasNext()) {
            Map.Entry<String, MethodSummary> me = it.next();
            MethodSummary methodSummary = me.getValue();
            printMethodSummaryHTML(pw, methodSummary);
        }
    }

    private void executeInvokeInstruction(JVMInvokeInstruction instruction, ThreadInfo threadInfo, Config configuration)
    {
        final String methodName = instruction.getInvokedMethodName();
        final MethodInfo methodInfo = instruction.getInvokedMethod();
        final StackFrame stackFrame = threadInfo.getTopFrame();
        if (!methodInfo.equals(stackFrame.getMethodInfo())) {
            return;
        }
        final ClassInfo classInfo = methodInfo.getClassInfo();
        final String className = classInfo.getName();
        final int numberOfArgs = instruction.getArgumentValues(threadInfo).length;
        if (( BytecodeUtils.isClassSymbolic(configuration, className, methodInfo, methodName) )
                || BytecodeUtils.isMethodSymbolic(configuration, methodInfo.getFullName(), numberOfArgs, null)) {
            MethodReader methodReader = MethodReader.createFor(threadInfo, instruction, methodInfo, classInfo);
            MethodSummary methodSummary = methodReader.createDescription();
            final String longName = methodInfo.getLongName();
            currentMethodName = longName;
            _methodSummaries.put(longName, methodSummary);
        }
    }

    private void executeReturnInstruction(VM vm, JVMReturnInstruction instruction, ThreadInfo threadInfo, Config conf)
    {
        final MethodInfo methodInfo = instruction.getMethodInfo();
        final ClassInfo classInfo = methodInfo.getClassInfo();
        if (classInfo == null) {
            return;
        }
        String className = classInfo.getName();
        String methodName = methodInfo.getName();
        int numberOfArgs = methodInfo.getNumberOfArguments();

        if (( ( BytecodeUtils.isClassSymbolic(conf, className, methodInfo, methodName) )
                || BytecodeUtils.isMethodSymbolic(conf, methodInfo.getFullName(), numberOfArgs, null) )) {
            PCChoiceGenerator choiceGenerator = searchForPCChoiceGenerator(vm.getChoiceGenerator());
            if (choiceGenerator == null) {
                return;
            }
            final PathCondition pathCondition = choiceGenerator.getCurrentPC();
            if (pathCondition == null) {
                return;
            }
            //pc.solve(); //we only solve the pc
            if (SymbolicInstructionFactory.concolicMode) { //TODO: cleaner
                SymbolicConstraintsGeneral solver = new SymbolicConstraintsGeneral();
                PCAnalyzer pa = new PCAnalyzer();
                pa.solve(pathCondition, solver);
            } else {
                pathCondition.solve();
            }

            if (!PathCondition.flagSolved) {
                return;
            }

            //after the following statement is executed, the pc loses its solution

            ReturnObject returnObject = processInstructions(instruction, threadInfo);

            //pc.solve();
            // not clear why this part is necessary
/*
                    if (SymbolicInstructionFactory.concolicMode) { //TODO: cleaner
                        SymbolicConstraintsGeneral solver = new SymbolicConstraintsGeneral();
                        PCAnalyzer pa = new PCAnalyzer();
                        pa.solve(pc,solver);
                    }
                    else
                        pc.solve();
*/

            String pcString = pathCondition.toString();
            Pair<String, String> pcPair = new Pair<String, String>(pcString, returnObject.getString());
            String longName = methodInfo.getLongName();
            MethodSummary methodSummary = _methodSummaries.get(longName);
            Vector<Pair> pcs = methodSummary.getPathConditions();
            if (( !pcs.contains(pcPair) ) && ( pcString.contains("SYM") )) {
                methodSummary.addPathCondition(pcPair);
            }

            if (_methodSummaries.get(longName) != null) { // recursive call
                longName = longName + methodSummary.hashCode(); // differentiate the key for recursive calls
            }
            _methodSummaries.put(longName, methodSummary);
            if (SymbolicInstructionFactory.debugMode) {
                System.out.println("*************Summary***************");
                System.out.println("PC is:" + pathCondition.toString());
                if (returnObject.getResult() != null) {
                    System.out.println("Return is:  " + returnObject.getResult());
                    System.out.println("***********************************");
                }
            }
        }
    }

    private ReturnObject processInstructions(JVMReturnInstruction instruction, ThreadInfo threadInfo)
    {
        if (instruction instanceof IRETURN) {
            IRETURN ireturn = (IRETURN)instruction;
            int returnValue = ireturn.getReturnValue();
            IntegerExpression returnAttr = (IntegerExpression)ireturn.getReturnAttr(threadInfo);
            if (returnAttr != null) {
                return ReturnObject.from(String.valueOf(returnAttr.solution()), returnAttr);
            } else { // concrete
                return ReturnObject.from(String.valueOf(returnValue), new IntegerConstant(returnValue));
            }
        } else if (instruction instanceof LRETURN) {
            LRETURN lreturn = (LRETURN)instruction;
            long returnValue = lreturn.getReturnValue();
            IntegerExpression returnAttr = (IntegerExpression)lreturn.getReturnAttr(threadInfo);
            if (returnAttr != null) {
                return ReturnObject.from(String.valueOf(returnAttr.solution()), returnAttr);
            } else { // concrete
                return ReturnObject.from(String.valueOf(returnValue), new IntegerConstant((int)returnValue));
            }
        } else if (instruction instanceof DRETURN) {
            DRETURN dreturn = (DRETURN)instruction;
            double returnValue = dreturn.getReturnValue();
            RealExpression returnAttr = (RealExpression)dreturn.getReturnAttr(threadInfo);
            if (returnAttr != null) {
                return ReturnObject.from(String.valueOf(returnAttr.solution()), returnAttr);
            } else { // concrete
                return ReturnObject.from(String.valueOf(returnValue), new RealConstant(returnValue));
            }
        } else if (instruction instanceof FRETURN) {
            FRETURN freturn = (FRETURN)instruction;
            double returnValue = freturn.getReturnValue();
            RealExpression returnAttr = (RealExpression)freturn.getReturnAttr(threadInfo);
            if (returnAttr != null) {
                return ReturnObject.from(String.valueOf(returnAttr.solution()), returnAttr);
            } else { // concrete
                return ReturnObject.from(String.valueOf(returnValue), new RealConstant(returnValue));
            }

        } else if (instruction instanceof ARETURN) {
            ARETURN areturn = (ARETURN)instruction;
            IntegerExpression returnAttr = (IntegerExpression)areturn.getReturnAttr(threadInfo);
            if (returnAttr != null) {
                return ReturnObject.from(String.valueOf(returnAttr.solution()), returnAttr);
            } else {// concrete
                Object val = areturn.getReturnValue(threadInfo);
                //DynamicElementInfo val = (DynamicElementInfo)areturn.getReturnValue(threadInfo);
                String tmp = String.valueOf(val);
                tmp = tmp.substring(tmp.lastIndexOf('.') + 1);
                return ReturnObject.from(String.valueOf(val), new SymbolicInteger(tmp));

            }
        } else {//other types of return
            return ReturnObject.from("-- ", null);
        }
    }


    //TODO:  needs to be changed not to use String representations
    private void printMethodSummary(PrintWriter pw, MethodSummary methodSummary)
    {
        System.out.println("Inputs: " + methodSummary.getSymValues());
        Vector<Pair> pathConditions = methodSummary.getPathConditions();
        if (pathConditions.size() > 0) {
            Iterator it = pathConditions.iterator();
            String allTestCases = EMPTY;
            while (it.hasNext()) {
                String testCase = methodSummary.getMethodName() + "(";
                Pair pcPair = (Pair)it.next();
                String pc = (String)pcPair._1;
                String errorMessage = (String)pcPair._2;
                String symValues = methodSummary.getSymValues();
                String argValues = methodSummary.getArgValues();
                String argTypes = methodSummary.getArgTypes();

                StringTokenizer st = new StringTokenizer(symValues, ",");
                StringTokenizer st2 = new StringTokenizer(argValues, ",");
                StringTokenizer st3 = new StringTokenizer(argTypes, ",");
                if (!argTypes.isEmpty() && argValues.isEmpty()) {
                    continue;
                }
                while (st2.hasMoreTokens()) {
                    String token = EMPTY;
                    String actualValue = st2.nextToken();
                    byte actualType = Byte.parseByte(st3.nextToken());
                    if (st.hasMoreTokens())
                        token = st.nextToken();
                    if (pc.contains(token)) {
                        String temp = pc.substring(pc.indexOf(token));
                        if (temp.indexOf(']') < 0) {
                            continue;
                        }

                        String val = temp.substring(temp.indexOf("[") + 1, temp.indexOf("]"));

                        //if(actualType == Types.T_INT || actualType == Types.T_FLOAT || actualType == Types.T_LONG || actualType == Types.T_DOUBLE)
                        //testCase = testCase + val + ",";
                        if (actualType == Types.T_INT || actualType == Types.T_FLOAT || actualType == Types.T_LONG || actualType == Types.T_DOUBLE) {
                            String suffix = EMPTY;
                            if (actualType == Types.T_LONG) {
                                suffix = "l";
                            } else if (actualType == Types.T_FLOAT) {
                                val = String.valueOf(Double.valueOf(val).floatValue());
                                suffix = "f";
                            }
                            if (val.endsWith("Infinity")) {
                                boolean isNegative = val.startsWith("-");
                                val = ( ( actualType == Types.T_DOUBLE ) ? "Double" : "Float" );
                                val += isNegative ? ".NEGATIVE_INFINITY" : ".POSITIVE_INFINITY";
                                suffix = EMPTY;
                            }
                            testCase = testCase + val + suffix + ",";
                        } else if (actualType == Types.T_BOOLEAN) { //translate boolean values represented as ints
                            //to "true" or "false"
                            if (val.equalsIgnoreCase("0"))
                                testCase = testCase + "false" + ",";
                            else
                                testCase = testCase + "true" + ",";
                        } else
                            throw new RuntimeException("## Error: listener does not support type other than int, long, float, double and boolean");
                        // TODO: to extend with arrays
                    } else {
                        //need to check if value is concrete
                        if (token.contains("CONCRETE"))
                            testCase = testCase + actualValue + ",";
                        else
                            testCase = testCase + SymbolicInteger.UNDEFINED + "(don't care),";// not correct in concolic mode
                    }
                }
                if (testCase.endsWith(","))
                    testCase = testCase.substring(0, testCase.length() - 1);
                testCase = testCase + ")";
                //process global information and append it to the output

                if (!errorMessage.equalsIgnoreCase(EMPTY))
                    testCase = testCase + "  --> " + errorMessage;
                //do not add duplicate test case
                if (!allTestCases.contains(testCase))
                    allTestCases = allTestCases + "\n" + testCase;
            }
            pw.println(allTestCases);
        } else {
            pw.println("No path conditions for " + methodSummary.getMethodName() +
                    "(" + methodSummary.getArgValues() + ")");
        }
    }

      /*
       * The way this method works is specific to the format of the methodSummary
       * data structure
       */

    private void printMethodSummaryHTML(PrintWriter pw, MethodSummary methodSummary)
    {
        pw.println("<h1>Test Cases Generated by Symbolic JavaPath Finder for " +
                methodSummary.getMethodName() + " (Path Coverage) </h1>");

        Vector<Pair> pathConditions = methodSummary.getPathConditions();
        if (pathConditions.size() > 0) {
            Iterator it = pathConditions.iterator();
            String allTestCases = EMPTY;
            String symValues = methodSummary.getSymValues();
            StringTokenizer st = new StringTokenizer(symValues, ",");
            while (st.hasMoreTokens())
                allTestCases = allTestCases + "<td>" + st.nextToken() + "</td>";
            allTestCases = "<tr>" + allTestCases + "<td>RETURN</td></tr>\n";
            while (it.hasNext()) {
                String testCase = "<tr>";
                Pair pcPair = (Pair)it.next();
                String pc = (String)pcPair._1;
                String errorMessage = (String)pcPair._2;
                //String _symValues = methodSummary.getSymValues();
                String argValues = methodSummary.getArgValues();
                String argTypes = methodSummary.getArgTypes();
                //StringTokenizer
                st = new StringTokenizer(symValues, ",");
                StringTokenizer st2 = new StringTokenizer(argValues, ",");
                StringTokenizer st3 = new StringTokenizer(argTypes, ",");
                while (st2.hasMoreTokens()) {
                    String token = EMPTY;
                    String actualValue = st2.nextToken();
                    byte actualType = Byte.parseByte(st3.nextToken());
                    if (st.hasMoreTokens())
                        token = st.nextToken();
                    if (pc.contains(token)) {
                        String temp = pc.substring(pc.indexOf(token));
                        if (temp.indexOf(']') < 0) {
                            continue;
                        }

                        String val = temp.substring(temp.indexOf("[") + 1, temp.indexOf("]"));
                        if (actualType == Types.T_INT || actualType == Types.T_FLOAT || actualType == Types.T_LONG || actualType == Types.T_DOUBLE)
                            testCase = testCase + "<td>" + val + "</td>";
                        else if (actualType == Types.T_BOOLEAN) { //translate boolean values represented as ints
                            //to "true" or "false"
                            if (val.equalsIgnoreCase("0"))
                                testCase = testCase + "<td>false</td>";
                            else
                                testCase = testCase + "<td>true</td>";
                        } else
                            throw new RuntimeException("## Error: listener does not support type other than int, long, float, double and boolean");

                    } else {
                        //need to check if value is concrete
                        if (token.contains("CONCRETE"))
                            testCase = testCase + "<td>" + actualValue + "</td>";
                        else
                            testCase = testCase + "<td>" + SymbolicInteger.UNDEFINED + "(don't care)</td>"; // not correct in concolic mode
                    }
                }

                //testCase = testCase + "</tr>";
                //process global information and append it to the output

                if (!errorMessage.equalsIgnoreCase(EMPTY))
                    testCase = testCase + "<td>" + errorMessage + "</td>";
                //do not add duplicate test case
                if (!allTestCases.contains(testCase))
                    allTestCases = allTestCases + testCase + "</tr>\n";
            }
            pw.println("<table border=1>");
            pw.print(allTestCases);
            pw.println("</table>");
        } else {
            pw.println("No path conditions for " + methodSummary.getMethodName() +
                    "(" + methodSummary.getArgValues() + ")");
        }

    }

    /**
     * Value class holding calculated results from {@link JVMReturnInstruction}.
     */
    private static class ReturnObject
    {
        private final Expression _result;

        private final String _returnString;

        private ReturnObject(String returnString, Expression result)
        {
            _returnString = returnString;
            _result = result;
        }

        String getString()
        {
            return _returnString;
        }

        Expression getResult(){
            return _result;
        }

        static ReturnObject from(String returnString, Expression result)
        {
            return new ReturnObject(String.format("Return value: %s", returnString), result);
        }
    }

    /**
     * Looks for {@link PCChoiceGenerator} in {@link ChoiceGenerator} chain and returns it, if does not find any then returning <b>null</b>
     *
     * @param choiceGenerator
     * @return
     */
    private PCChoiceGenerator searchForPCChoiceGenerator(ChoiceGenerator<?> choiceGenerator)
    {
        if (!( choiceGenerator instanceof PCChoiceGenerator )) {
            ChoiceGenerator<?> previous = choiceGenerator.getPreviousChoiceGenerator();
            while (!( ( previous == null ) || ( previous instanceof PCChoiceGenerator ) )) {
                previous = previous.getPreviousChoiceGenerator();
            }
            choiceGenerator = previous;
        }
        return choiceGenerator == null ? null : (PCChoiceGenerator)choiceGenerator;
    }

    /**
     * Reads method description from {@link ClassInfo}, {@link JVMInvokeInstruction}, {@link MethodInfo} and {@link ThreadInfo} in order to create
     * appropriate
     * {@link gov.nasa.jpf.symbc.SymbolicListener.MethodSummary} class.
     */
    protected static class MethodReader
    {

        private final ClassInfo _clazz;

        private final JVMInvokeInstruction _instruction;

        private final MethodInfo _method;

        private final ThreadInfo _thread;

        private MethodReader(ThreadInfo thread, JVMInvokeInstruction instruction, MethodInfo method, ClassInfo clazz)
        {

            _thread = thread;
            _instruction = instruction;
            _method = method;
            _clazz = clazz;
        }

        static MethodReader createFor(ThreadInfo thread, JVMInvokeInstruction instruction, MethodInfo method, ClassInfo clazz)
        {
            return new MethodReader(thread, instruction, method, clazz);
        }

        /**
         * Creates a {@link gov.nasa.jpf.symbc.SymbolicListener.MethodSummary} reflects given info.
         *
         * @return
         */
        MethodSummary createDescription()
        {
            MethodSummary.Builder builder = MethodSummary.Builder.newBuilder();
            builder.name(createMethodName());
            Object[] argValues = _instruction.getArgumentValues(_thread);
            final String argValuesStr = readArgumentValues(argValues);
            builder.argumentValues(argValuesStr);
            byte[] argTypes = _method.getArgumentTypes();
            final String argTypesStr = readTypes(argTypes);
            builder.argumentTypes(argTypesStr);

            //get the symbolic values (changed from constructing them here)
            final int numberOfArgs = _instruction.getArgumentValues(_thread).length;
            final String symValuesStr = readSymbolicValues(_instruction, _method, _thread.getTopFrame(), numberOfArgs, argTypes);

            builder.symbolicValues(symValuesStr);
            return builder.build();
        }

        private static String getShortMethodName(String methodName)
        {
            if (methodName.contains("(")) {
                return methodName.substring(0, methodName.indexOf("("));
            }
            return methodName;
        }

        private static String readArgumentValues(Object[] argValues)
        {
            StringBuilder builder = new StringBuilder(EMPTY);
            for (int i = 0; i < argValues.length; i++) {
                builder.append(argValues[i]);
                if (( i + 1 ) < argValues.length) {
                    builder.append(",");
                }
            }
            return builder.toString();
        }

        private static String readSymbolicValues(JVMInvokeInstruction invokeInstruction, MethodInfo methodInfo, StackFrame sf, int numberOfArgs,
                byte[]
                        argTypes)
        {
            String symValuesStr = EMPTY;
            String symVarNameStr = EMPTY;

            LocalVarInfo[] argsInfo = methodInfo.getArgumentLocalVars();
            // TODO should be fixed, see https://groups.google.com/forum/#!topic/java-pathfinder/jhOkvLx-SKE
            if (argsInfo == null) {
                throw new RuntimeException("ERROR: you need to turn debug option on");
            }

            int sfIndex = 1; //do not consider implicit param "this"
            int namesIndex = 1;
            if (invokeInstruction instanceof INVOKESTATIC) {
                sfIndex = 0; // no "this" for static
                namesIndex = 0;
            }

            for (int i = 0; i < numberOfArgs; i++) {
                Expression expLocal = (Expression)sf.getLocalAttr(sfIndex);
                if (expLocal != null) // symbolic
                {
                    symVarNameStr = expLocal.toString();
                } else {
                    symVarNameStr = argsInfo[namesIndex].getName() + "_CONCRETE" + ",";
                }
                // TODO: what happens if the argument is an array?
                symValuesStr = symValuesStr + symVarNameStr + ",";
                sfIndex++;
                namesIndex++;
                if (argTypes[i] == Types.T_LONG || argTypes[i] == Types.T_DOUBLE)
                    sfIndex++;

            }

            // get rid of last ","
            if (symValuesStr.endsWith(",")) {
                symValuesStr = symValuesStr.substring(0, symValuesStr.length() - 1);
            }
            return symValuesStr;
        }

        private static String readTypes(byte[] argTypes)
        {
            StringBuilder builder = new StringBuilder(EMPTY);
            for (int i = 0; i < argTypes.length; i++) {
                builder.append(argTypes[i]);
                if (( i + 1 ) < argTypes.length) {
                    builder.append(",");
                }
            }
            return builder.toString();
        }

        private String createMethodName()
        {
            final String shortName = getShortMethodName(_instruction.getInvokedMethodName());
            return _clazz.getName() + "." + shortName;
        }
    }

    protected static class MethodSummary
    {
        private final String _argTypes;

        private final String _argValues;

        private final String _methodName;

        private final Vector<Pair> _pathConditions = new Vector<Pair>();

        private final String _symValues;

        private MethodSummary(String methodName, String argumentTypes, String argumentValues, String symbolicValues)
        {
            _methodName = methodName;
            _argTypes = argumentTypes;
            _argValues = argumentValues;
            _symValues = symbolicValues;
        }

        public void addPathCondition(Pair pc)
        {
            _pathConditions.add(pc);
        }

        public String getArgTypes()
        {
            return _argTypes;
        }

        public String getArgValues()
        {
            return _argValues;
        }

        public String getMethodName()
        {
            return _methodName;
        }

        public Vector<Pair> getPathConditions()
        {
            return _pathConditions;
        }

        public String getSymValues()
        {
            return _symValues;
        }

        static class Builder
        {
            private String _argTypes = EMPTY;

            private String _argValues = EMPTY;

            private String _methodName = EMPTY;

            private String _symValues = EMPTY;

            private Vector<Pair> pathConditions = new Vector<>();

            static Builder newBuilder()
            {
                return new Builder();
            }

            Builder argumentTypes(String argumentTypes)
            {
                _argTypes = argumentTypes;
                return this;
            }

            Builder argumentValues(String argumentValues)
            {
                _argValues = argumentValues;
                return this;
            }

            MethodSummary build()
            {
                return new MethodSummary(_methodName, _argTypes, _argValues, _symValues);
            }

            Builder name(String methodName)
            {
                _methodName = methodName;
                return this;
            }

            Builder symbolicValues(String symbolicValues)
            {
                _symValues = symbolicValues;
                return this;
            }
        }
    }
}
