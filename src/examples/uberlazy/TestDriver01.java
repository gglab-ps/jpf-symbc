//
// Copyright (C) 2006 United States Government as represented by the
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
//
package uberlazy;

/**
 * @author Neha Rungta
 * 
 * This class tests whether all the non-deterministic choices arising
 * from polymorphism are accounted for. The initialization of the 
 * data structure n is all the classes that Node is an instanceof
 * It also serves as a test case when for the partition function 
 * at the non-deterministic choice of the instanceof class
 * 
 * Without the polymorphism only the second print statement will 
 * be executed. With polymorphism the first statements prints twice
 * and the else prints once. With the correct partition function
 * each statement prints once. 
 **/

import gov.nasa.jpf.symbc.Symbolic;


public class TestDriver01 {
	
	@Symbolic("true")
	Node n;

	
	public void run () {
		if(n != null) {
			if(n instanceof dblNode) {
				System.out.println("You can store reals in this data structure");
			} else {
				System.out.println("Don't store a Real in here");
			}
		}
	}
	
	public static void main(String[] args) {
		TestDriver01 tt = new TestDriver01();
		tt.run();
	}
	
}