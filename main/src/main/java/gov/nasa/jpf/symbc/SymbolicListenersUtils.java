/*
* $Id$
*
* (C) Copyright ParaSoft Corporation 2015. All rights reserved.
* THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF ParaSoft
* The copyright notice above does not evidence any
* actual or intended publication of such source code.
*/

package gov.nasa.jpf.symbc;

import gov.nasa.jpf.symbc.concolic.PCAnalyzer;
import gov.nasa.jpf.symbc.numeric.IntegerExpression;
import gov.nasa.jpf.symbc.numeric.PCChoiceGenerator;
import gov.nasa.jpf.symbc.numeric.PathCondition;
import gov.nasa.jpf.symbc.numeric.RealExpression;
import gov.nasa.jpf.symbc.numeric.SymbolicConstraintsGeneral;
import gov.nasa.jpf.symbc.sequences.SequenceChoiceGenerator;
import gov.nasa.jpf.symbc.string.StringSymbolic;
import gov.nasa.jpf.vm.ChoiceGenerator;
import gov.nasa.jpf.vm.MethodInfo;
import gov.nasa.jpf.vm.StackFrame;
import gov.nasa.jpf.vm.Types;

import java.util.ArrayList;
import java.util.List;

/**
 * Common util method used in symbc listeners
 * <p>
 * Created by grzesuav on 02.04.15.
 */
public class SymbolicListenersUtils
{
    public static final String EMPTY = "";

    private SymbolicListenersUtils()
    {
        // to prevent instantiations
    }

    /**
     * Returns symbolic attributes of given method.
     * TODO: fix there concretely executed method will have null attributes.
     *
     * @param methodInfo
     * @param stackFrame
     * @param numberOfArguments
     * @param isStatic
     * @return
     */
    public static Object[] getArgumentAttributes(final MethodInfo methodInfo, final StackFrame stackFrame, int numberOfArguments, boolean isStatic)
    {
        Object[] attributes = new Object[numberOfArguments];
        byte[] argumentTypes = methodInfo.getArgumentTypes();
        assert methodInfo.isStatic() == isStatic;
        int count = 1; // we do not care about this
        if (isStatic) {
            count = 0;  //no "this" reference
        }
        for (int i = 0; i < numberOfArguments; i++) {
            attributes[i] = stackFrame.getLocalAttr(count);
            count++;
            if (argumentTypes[i] == Types.T_LONG || argumentTypes[i] == Types.T_DOUBLE) {
                count++;
            }
        }
        return attributes;
    }

    /**
     * A single invoked 'method' is represented as a String.
     * information about the invoked method is got from the SequenceChoiceGenerator
     */
    public static String getInvokedMethod(SequenceChoiceGenerator choiceGenerator)
    {
        // get method name
        String shortName = choiceGenerator.getMethodShortName();
        StringBuilder invokedMethod = new StringBuilder(shortName).append("(");

        // get argument values
        Object[] argValues = choiceGenerator.getArgValues();

        // get number of arguments
        int numberOfArgs = argValues.length;

        // get symbolic attributes
        Object[] attributes = choiceGenerator.getArgAttributes();

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
                        if (( (IntegerExpression)e ).solution() == 0) {
                            solution = solution + "false";
                        } else {
                            solution = solution + "true";
                        }
                    } else
                        solution = solution + ( (IntegerExpression)e ).solution();
                } else if (e instanceof RealExpression) {
                    solution = solution + ( (RealExpression)e ).solution();
                } else {
                    solution = solution + ( (StringSymbolic)e ).solution();
                }
                invokedMethod.append(solution);
            } else { // parameter concrete - for a concrete parameter, the symbolic attribute is null
                invokedMethod.append(argValues[i]);
            }
            if (i + 1 < numberOfArgs) { //add comma between arguments
                invokedMethod.append(",");
            }
        }
        invokedMethod.append(")");

        return invokedMethod.toString();
    }

    /**
     * traverses the {@link ChoiceGenerator} chain to get the method sequence
     * looks for {@link SequenceChoiceGenerator} in the chain
     * SequenceChoiceGenerators have information about the methods
     * executed and hence the method sequence can be obtained.
     * A single 'methodSequence' is a vector of invoked 'method's along a path
     * A single invoked 'method' is represented as a String.
     */
    public static List<String> getMethodSequence(ChoiceGenerator[] cgs)
    {
        // A method sequence is a vector of strings
        List<String> methodSequence = new ArrayList<>();
        // explore the choice generator chain - unique for a given path.
        for (ChoiceGenerator choiceGenerator : cgs) {
            if (( choiceGenerator instanceof SequenceChoiceGenerator )) {
                methodSequence.add(getInvokedMethod((SequenceChoiceGenerator)choiceGenerator));
            }
        }
        return methodSequence;
    }

    /**
     * Looks for {@link PCChoiceGenerator} in {@link ChoiceGenerator} chain and returns it, if does not find any then returning <b>null</b>
     *
     * @param choiceGenerator
     * @return
     */
    public static PCChoiceGenerator searchForPCChoiceGenerator(final ChoiceGenerator<?> choiceGenerator)
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

    /**
     * Solve given {@link PathCondition} depends on configured mode.
     *
     * @param pathCondition condition to solve
     */
    public static void solve(PathCondition pathCondition)
    {
        if (SymbolicInstructionFactory.concolicMode) { //TODO: cleaner
            SymbolicConstraintsGeneral solver = new SymbolicConstraintsGeneral();
            PCAnalyzer pa = new PCAnalyzer();
            pa.solve(pathCondition, solver);
        } else {
            pathCondition.solve();
        }
    }
}
