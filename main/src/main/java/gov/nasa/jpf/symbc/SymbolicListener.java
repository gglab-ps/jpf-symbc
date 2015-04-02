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
import java.util.*;
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
            //	SystemState ss = vm.getSystemState();
            Config conf = vm.getConfig();
            if (executedInstruction instanceof JVMInvokeInstruction) {
                executeInvokeInstruction((JVMInvokeInstruction)executedInstruction, currentThread, conf);
            } else if (executedInstruction instanceof JVMReturnInstruction) {
                executeReturnInstruction(vm, (JVMReturnInstruction)executedInstruction, currentThread, conf);
            }
        }
    }

    @Override
    public void propertyViolated(Search search)
    {

        VM vm = search.getVM();
        final PCChoiceGenerator choiceGenerator = searchForPCChoiceGenerator(vm.getChoiceGenerator());
        if (choiceGenerator == null) {
            return;
        }
        PathCondition pathCondition = choiceGenerator.getCurrentPC();
        if (pathCondition == null) {
            return;
        }
        String error = search.getLastError().getDetails();
        error = "\"" + error.substring(0, error.indexOf("\n")) + "...\"";
        // C: not clear where result was used here -- to review
        //PathCondition result = new PathCondition();
        //IntegerExpression sym_err = new SymbolicInteger("ERROR");
        //IntegerExpression sym_value = new SymbolicInteger(error);
        //result._addDet(Comparator.EQ, sym_err, sym_value);
        //solve the path condition, then print it
        //pc.solve();
        solve(pathCondition);

        Pair<PathCondition, String> pcPair = new Pair<PathCondition, String>(pathCondition, error);//(pc.toString(),error);

        //String _methodName = vm.getLastInstruction().getMethodInfo().getName();
        MethodSummary methodSummary = _methodSummaries.get(currentMethodName);
        methodSummary.addPathCondition(pcPair);
        System.out.println("Property Violated: PC is " + pathCondition.toString());
        System.out.println("Property Violated: result is  " + error);
        System.out.println("****************************");
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
            pw.println();
            pw.println();
            pw.println();
            Map.Entry<String, MethodSummary> me = it.next();
            MethodSummary methodSummary = me.getValue();
            Printer printer = new Printer(pw, methodSummary);
            printer.printSummary();
        }

        //        publisher.publishTopicStart("Method Summaries (HTML)");
        //        it = _methodSummaries.entrySet().iterator();
        //        while (it.hasNext()) {
        //            Map.Entry<String, MethodSummary> me = it.next();
        //            MethodSummary methodSummary = me.getValue();
        //            printMethodSummaryHTML(pw, methodSummary);
        //        }
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

            solve(pathCondition);

            if (!PathCondition.flagSolved) {
                return;
            }

            //after the following statement is executed, the pc loses its solution

            ReturnObject returnObject = processInstructions(instruction, threadInfo);

            Pair<PathCondition, String> pcPair = new Pair<PathCondition, String>(pathCondition, returnObject.getString());
            String longName = methodInfo.getLongName();
            MethodSummary methodSummary = _methodSummaries.get(longName);
            List<Pair<PathCondition, String>> pcs = methodSummary.getPathConditions();
            if (( !pcs.contains(pcPair) ) && ( pathCondition.toString().contains("SYM") )) {
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


    private void printMethodSummaryHTML(PrintWriter pw, MethodSummary methodSummary)
    {
        pw.println("<h1>Test Cases Generated by Symbolic JavaPath Finder for " +
                methodSummary.getMethodName() + " (Path Coverage) </h1>");

        List<Pair<PathCondition, String>> pathConditions = methodSummary.getPathConditions();
        if (pathConditions.size() > 0) {
            Iterator<Pair<PathCondition, String>> it = pathConditions.iterator();
            String allTestCases = EMPTY;
            String symValues = methodSummary.getSymValues();
            StringTokenizer st = new StringTokenizer(symValues, ",");
            while (st.hasMoreTokens())
                allTestCases = allTestCases + "<td>" + st.nextToken() + "</td>";
            allTestCases = "<tr>" + allTestCases + "<td>RETURN</td></tr>\n";
            while (it.hasNext()) {
                String testCase = "<tr>";
                Pair<PathCondition, String> pcPair = it.next();
                PathCondition pc = pcPair._1;
                String errorMessage = pcPair._2;
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
                    if (pc.toString().contains(token)) {
                        String temp = pc.toString().substring(pc.toString().indexOf(token));
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

    /**
     * Looks for {@link PCChoiceGenerator} in {@link ChoiceGenerator} chain and returns it, if does not find any then returning <b>null</b>
     *
     * @param choiceGenerator
     * @return
     */
    private PCChoiceGenerator searchForPCChoiceGenerator(final ChoiceGenerator<?> choiceGenerator)
    {
        if (choiceGenerator instanceof PCChoiceGenerator) {
            return (PCChoiceGenerator)choiceGenerator;
        }
        ChoiceGenerator<?> previous = choiceGenerator.getPreviousChoiceGenerator();
        while (!( ( previous == null ) || ( previous instanceof PCChoiceGenerator ) )) {
            previous = previous.getPreviousChoiceGenerator();
        }
        return previous == null ? null : (PCChoiceGenerator)previous;
    }

      /*
       * The way this method works is specific to the format of the methodSummary
       * data structure
       */

    private void solve(PathCondition pathCondition)
    {
        if (SymbolicInstructionFactory.concolicMode) { //TODO: cleaner
            SymbolicConstraintsGeneral solver = new SymbolicConstraintsGeneral();
            PCAnalyzer pa = new PCAnalyzer();
            pa.solve(pathCondition, solver);
        } else {
            pathCondition.solve();
        }
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

        private final List<Pair<PathCondition, String>> _pathConditions = new ArrayList<>();

        private final String _symValues;

        private MethodSummary(String methodName, String argumentTypes, String argumentValues, String symbolicValues)
        {
            _methodName = methodName;
            _argTypes = argumentTypes;
            _argValues = argumentValues;
            _symValues = symbolicValues;
        }

        public void addPathCondition(Pair<PathCondition, String> pc)
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

        public List<Pair<PathCondition, String>> getPathConditions()
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

    static class Printer
    {

        private final MethodSummary _summary;

        private final PrintWriter _writer;

        Printer(PrintWriter writer, MethodSummary summary)
        {

            _writer = writer;
            _summary = summary;
        }

        void printSummary()
        {
            printBegin();
            printBody();
        }

        private String collectCondition(PathCondition pathCondition, String sMessage)
        {
            String[] aSymbolicValues = _summary.getSymValues().split(",");
            String[] aArgValues = _summary.getArgValues().split(",");
            String[] aArgTypes = _summary.getArgTypes().split(",");
            if (aArgTypes.length != aArgValues.length) {
                return EMPTY;
            }
            final StringBuilder builder = new StringBuilder(_summary.getMethodName());
            builder.append('(');
            for (int i = 0; i < aArgTypes.length; i++) {
                if (isSymbolic(pathCondition, aSymbolicValues[i])) {
                    String stringCondition = pathCondition.toString();
                    String temporary = stringCondition.substring(stringCondition.indexOf(aSymbolicValues[i]));
                    int closingBraceIndex = temporary.indexOf(']');
                    if (closingBraceIndex < 0) {
                        continue;
                    }
                    int openingBraceIndex = temporary.indexOf('[');
                    String value = temporary.substring(openingBraceIndex + 1, closingBraceIndex);
                    builder.append(getValue(value, aArgTypes[i]));
                } else if (isConcrete(aSymbolicValues[i])) {
                    builder.append(aArgValues[i]);
                } else {// undefined
                    builder.append(SymbolicInteger.UNDEFINED).append("(don't care)");
                }
                if (i + 1 < aArgTypes.length) {
                    builder.append(',');
                }
            }
            builder.append(')');
            if (!sMessage.equals(EMPTY)) {
                builder.append(" --> ").append(sMessage);
            }
            return builder.toString();
        }

        private String getInfinityValue(String value, byte type)
        {
            StringBuilder builder = new StringBuilder();
            switch (type) {
                case Types.T_DOUBLE: {
                    builder.append("Double");
                    break;
                }
                case Types.T_FLOAT: {
                    builder.append("Float");
                    break;
                }
                default: {
                    builder.append("Non-expected type: ").append(type);
                    break;
                }
            }
            boolean negative = value.charAt(0) == '-';
            if (negative) {
                builder.append(".NEGATIVE_INFINITY");
            } else {
                builder.append(".POSITIVE_INFINITY");
            }
            return builder.toString();
        }

        private String getValue(String value, String aArgType)
        {
            final byte type = Byte.parseByte(aArgType);
            if (value.endsWith("Infinity")) {
                return getInfinityValue(value, type);
            }
            switch (type) {
                case Types.T_INT: {
                    return value;
                }
                case Types.T_LONG: {
                    return value + 'l';
                }
                case Types.T_FLOAT: {
                    return Float.toString(Double.valueOf(value).floatValue()) + 'f';
                }
                case Types.T_DOUBLE: {
                    return Double.valueOf(value).toString() + 'd';
                }
                case Types.T_BOOLEAN: {
                    return value.equals('0') ? Boolean.FALSE.toString() : Boolean.TRUE.toString();
                }
                default: {
                    throw new RuntimeException("## Error: listener does not support type other than int, long, float, double and boolean");
                }
            }
        }

        private boolean isConcrete(String aSymbolicValue)
        {
            return aSymbolicValue.contains("CONCRETE");
        }

        private boolean isSymbolic(PathCondition condition, String symbolicValue)
        {
            return condition.toString().contains(symbolicValue);
        }

        private void printBegin()
        {
            _writer.println(String.format("Summary for method: %s", _summary.getMethodName()));
            _writer.println(String.format("Inputs: %s", _summary.getSymValues()));
        }

        private void printBody()
        {
            final List<Pair<PathCondition, String>> pathConditions = _summary.getPathConditions();
            if (pathConditions.isEmpty()) {
                printEmpty();
            } else {
                printConditions(pathConditions);
            }
        }

        private void printConditions(List<Pair<PathCondition, String>> pathConditions)
        {
            Set<String> conditions = new HashSet<>();
            for (Pair<PathCondition, String> pathCondition : pathConditions) {
                String sCondition = collectCondition(pathCondition._1, pathCondition._2);
                conditions.add(sCondition);
            }
            for (String sCondition : conditions) {
                _writer.println(sCondition);
            }
        }

        private void printEmpty()
        {
            _writer.println(String.format("No path conditions for %s (%s)", _summary.getMethodName(), _summary.getArgValues()));
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

        static ReturnObject from(String returnString, Expression result)
        {
            return new ReturnObject(String.format("Return value: %s", returnString), result);
        }

        Expression getResult()
        {
            return _result;
        }

        String getString()
        {
            return _returnString;
        }
    }
}
