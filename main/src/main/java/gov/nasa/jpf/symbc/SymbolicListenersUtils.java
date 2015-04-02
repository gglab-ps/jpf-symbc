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
import gov.nasa.jpf.symbc.numeric.PCChoiceGenerator;
import gov.nasa.jpf.symbc.numeric.PathCondition;
import gov.nasa.jpf.symbc.numeric.SymbolicConstraintsGeneral;
import gov.nasa.jpf.vm.ChoiceGenerator;
import gov.nasa.jpf.vm.MethodInfo;
import gov.nasa.jpf.vm.StackFrame;
import gov.nasa.jpf.vm.Types;

/**
 * Common util method used in symbc listeners
 * <p>
 * Created by grzesuav on 02.04.15.
 */
public class SymbolicListenersUtils
{
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
