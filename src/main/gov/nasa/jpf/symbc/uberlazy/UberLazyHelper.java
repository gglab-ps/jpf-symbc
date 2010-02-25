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
package gov.nasa.jpf.symbc.uberlazy;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Set;

import gov.nasa.jpf.jvm.ChoiceGenerator;
import gov.nasa.jpf.jvm.ClassInfo;
import gov.nasa.jpf.jvm.DynamicArea;
import gov.nasa.jpf.jvm.DynamicElementInfo;
import gov.nasa.jpf.jvm.ElementInfo;
import gov.nasa.jpf.jvm.FieldInfo;
import gov.nasa.jpf.jvm.Fields;
import gov.nasa.jpf.jvm.KernelState;
import gov.nasa.jpf.jvm.ThreadInfo;
import gov.nasa.jpf.symbc.heap.HeapNode;
import gov.nasa.jpf.symbc.heap.Helper;
import gov.nasa.jpf.symbc.heap.SymbolicInputHeap;
import gov.nasa.jpf.symbc.numeric.Comparator;
import gov.nasa.jpf.symbc.numeric.IntegerConstant;
import gov.nasa.jpf.symbc.numeric.PathCondition;
import gov.nasa.jpf.symbc.numeric.SymbolicInteger;

public class UberLazyHelper {
	
	public static boolean symbolicVariableExists(ChoiceGenerator<?> cg, int objref) {
		EquivalenceObjects equivObjs = ((PartitionChoiceGenerator) cg).
														getCurrentEquivalenceObject();
		if(equivObjs != null && equivObjs.getEquivClass(objref) != null) {
			return true;
		}
		return false;
	}
	
	public static HashMap<String, Set<String>> checkTypesForPrimitiveFields
									(ChoiceGenerator<?> cg, int objref, FieldInfo fi) {	
		HashMap<String, Set<String>> newPartitions = 
										new HashMap<String, Set<String>>();
		EquivalenceObjects equivObjs = ((PartitionChoiceGenerator) cg).
														getCurrentEquivalenceObject();
		EquivalenceClass eqClass = equivObjs.getEquivClass(objref);
		ArrayList<EquivalenceElem> elements = eqClass.getElementsInEquivClass();
		for(int elemIndex = 0; elemIndex < elements.size(); elemIndex++) {
			EquivalenceElem elem = elements.get(elemIndex);
			ClassInfo orig = ClassInfo.getResolvedClassInfo(elem.getTypeOfElement());
			boolean found = findMatchingFields(orig, orig, fi,newPartitions);
			ClassInfo ci = orig;
			// go up the class hierarchy to find the field 
			// when it is not found in the current class in java
			// since one class can inherit from a single class this works
			while(!found) {
				found = findMatchingFields(ci, orig, fi, newPartitions);
				ci = ci.getSuperClass();
			}
			//System.out.println("=================================");
		}
		//System.out.println(newPartitions.toString());
		//System.exit(1);
		return newPartitions;
	}
	
	private static boolean findMatchingFields(ClassInfo ci, ClassInfo origCI, FieldInfo fi,
										HashMap<String, Set<String>> newPartitions) {
		boolean found = false;
		FieldInfo[] fields = ci.getDeclaredInstanceFields();
		for(int fieldIndex = 0; fieldIndex < fields.length; fieldIndex++) {
			//System.out.println(fields[fieldIndex].getName() + "--- fieldName");
			if(fields[fieldIndex].getName().equals(fi.getName())) {
				found = true;
				String typeKey = fields[fieldIndex].getType();
				Set<String> partition;
				if(newPartitions.containsKey(typeKey)) {
					 partition = newPartitions.get(typeKey);
				} else {
					 partition = new HashSet<String>();
				}
				partition.add(origCI.getName());
				newPartitions.put(typeKey, partition);
			}
		}
		return found;
	}
	
	 public static HashMap<Integer, EquivalenceClass> initializePartitionDataStructs
	 									(String objIdentifier, int numPartitions) {
		 HashMap<Integer, EquivalenceClass> partition = new 
		 							HashMap<Integer, EquivalenceClass>(numPartitions);
		 for(int index = 0; index < numPartitions; index++) {
			EquivalenceClass ec = new EquivalenceClass(objIdentifier);
			partition.put(index, ec);
		 }
		 return partition;
	 }
	 
	 public static HashMap<Integer, EquivalenceClass> initializePartitionDataStructs
	 												(int objIdentifier, int numPartitions) {
		return UberLazyHelper.initializePartitionDataStructs
										(Integer.toString(objIdentifier),numPartitions);
	 }
	 
	 public static HashMap<Integer, EquivalenceClass> initializePartitionsWithData(int objId,
			 							HashMap<String, Set<String>> partFunc) {
		 int numPartitions = partFunc.size();
		 return UberLazyHelper.initializePartitionDataStructs(objId, numPartitions);
	 }
	 
	 public static EquivalenceElem getSuperParentInClassHierarchy(EquivalenceClass eqClass, int objRef) {
		 ArrayList<EquivalenceElem> eqElems = eqClass.getElementsInEquivClass();
		 return UberLazyHelper.getSuperParentInClassHeirarchy(eqElems, objRef);
	 }
	 
	 public static EquivalenceElem getSuperParentInClassHeirarchy(ArrayList<EquivalenceElem> eqElems, int objRef) {
		 if(eqElems.size() <= 0) {
			 return null;
		 }
		 EquivalenceElem elem = eqElems.get(0);
		 ClassInfo parentClassInfo = ClassInfo.getResolvedClassInfo
		 												(elem.getTypeOfElement());

		 for(int eqIndex = 1; eqIndex < eqElems.size(); eqIndex++) {
			 EquivalenceElem currElem = eqElems.get(eqIndex);
			 if(parentClassInfo.isInstanceOf(currElem.getTypeOfElement())) {
				 parentClassInfo = ClassInfo.getResolvedClassInfo
				 										(currElem.getTypeOfElement());
				 // found the parent again; might arise due to aliased equivalence elements
				 if(elem.getTypeOfElement().equals(currElem.getTypeOfElement())) {   
					 if(objRef != -1 && currElem.getAliasIdentifier().equals(String.valueOf(objRef))) {
						 // replace only if the parent is from the current objRef, if anything
						 // matches the objref that is given priority
						 elem = currElem;
					 } // otherwise do nothing
				 } else { 
					 elem = currElem;
				 }
			 } 	

		 }
		 return elem;
	 }

	 public static ChoiceGenerator<?> getPrevPartitionChoiceGenerator(ChoiceGenerator<?> currCG) {
		 ChoiceGenerator<?> prevPartitionCG = currCG;
		 while(!((prevPartitionCG == null) || (prevPartitionCG instanceof 
				 PartitionChoiceGenerator))) {
			 prevPartitionCG = prevPartitionCG.getPreviousChoiceGenerator();
		 }
		 return prevPartitionCG;

	 }
	 
	 public static EquivalenceObjects getEquivalenceObjects(ChoiceGenerator<?> prevPartitionCG, int objRef) {
		 if(prevPartitionCG != null && 
				 UberLazyHelper.symbolicVariableExists(prevPartitionCG, objRef)) {
			 return ((PartitionChoiceGenerator) prevPartitionCG).
			 									getCurrentEquivalenceObject();
		 }
		 return null;
	 }
	 
	 public static EquivalenceObjects generateNewEquivalenceClass(EquivalenceObjects currEquivObjs, int objref,
			 													ArrayList<EquivalenceElem> equivElems) {
		 EquivalenceClass eqClass = new EquivalenceClass(Integer.toString(objref), equivElems);
		 currEquivObjs.replaceClass(objref, eqClass);
		 return currEquivObjs;
	 }
	 
	 public static ArrayList<EquivalenceElem> getAllAliasedObjects(ChoiceGenerator<?> prevCG, String typeClassInfo) {

		 ArrayList<EquivalenceElem> aliasedElem = new ArrayList<EquivalenceElem>();
		 //get all the objects that were declared on the symbolic input heap
		 if(prevCG == null) {
			 return aliasedElem;
		 }
		// System.out.println("prevHeap is not null");
		 SymbolicInputHeap symInputHeap = ((PartitionChoiceGenerator) prevCG).
		 													getCurrentSymInputHeap();
		 EquivalenceObjects equivObjs = ((PartitionChoiceGenerator) prevCG).
		 													getCurrentEquivalenceObject();
		 HeapNode n = symInputHeap.header();
		 while(null != n) {
			 ClassInfo tClassInfo = n.getType();
			 int daIndex = n.getIndex();
			 EquivalenceClass eqClass = null;
			 if(equivObjs.containsEquivClassForRef(daIndex)) {
					eqClass = equivObjs.getEquivClass(daIndex);
					ArrayList<EquivalenceElem> elems = eqClass.
														getElementsInEquivClass();
				
					// if the parent is an instance, all its subtypes will 
					if(tClassInfo.isInstanceOf(typeClassInfo)){
						aliasedElem.addAll(elems);
					} else {
						// if the parent is not an instanceof, one or more of
						// subtypes maybe eligible to be put in the equivlance
						// class
						for(int elemIndex = 0; elemIndex < elems.size(); elemIndex++) {
							EquivalenceElem anElem = elems.get(elemIndex);
							ClassInfo elemClassInfo = ClassInfo.getResolvedClassInfo
																		(anElem.typeOfElem);
							if(elemClassInfo.isInstanceOf(typeClassInfo)) {
								aliasedElem.add(anElem);
							}
						}
					}
			 }
			 n = n.getNext();
		 }
		 //System.out.println(aliasedElem.toString());
		 return aliasedElem;
	 }
	 
	 //initializes a new concretization for object represented by EqivalenceElem -- "elem"
	 public static void generatingNewConcretization(int objref, EquivalenceElem elem,
			 SymbolicInputHeap symInputHeap, KernelState ks, ThreadInfo th) {
		 HeapNode n = symInputHeap.header();
		 while(null != n) {
			 if(objref == n.getIndex()) {
				 ClassInfo tClassInfo = ClassInfo.
				 					getResolvedClassInfo(elem.getTypeOfElement());
				 //replace in the symbolic world
				 n.replaceType(tClassInfo);
				 
				 //replace in the concrete world
				 DynamicElementInfo dei = ks.da.get(objref);
				 dei.restoreFields(tClassInfo.createInstanceFields());	
				 int daIndex = objref; 
				 ElementInfo eiRef = DynamicArea.getHeap().get(daIndex);
				 
				 //initialize the instance fields as symbolic 
				 Fields f = eiRef.getFields();
				 int numOfFields = f.getNumberOfFields();
				 FieldInfo[] fields = new FieldInfo[numOfFields];
				 for(int fieldIndex = 0; fieldIndex < numOfFields; fieldIndex++) {
					 fields[fieldIndex] = f.getFieldInfo(fieldIndex);
				 }
				 String refChain; 
				 if(n instanceof UberLazyHeapNode) {
					 UberLazyHeapNode hn  = (UberLazyHeapNode) n;
					 refChain = hn.getRefChain();
				 } else {
					 refChain = "";
				 }
				 Helper.initializeInstanceFields(fields, eiRef,refChain);

				 //initialize the static fields as symbolic 					
				 ClassInfo superClass = tClassInfo;
				 while(superClass != null) {
					 FieldInfo[] staticFields = superClass.getDeclaredStaticFields();
					 Helper.initializeStaticFields(staticFields, superClass, th);
					 superClass = superClass.getSuperClass();
				 }
				 
				 //TODO: Update the heap constraint based on what is split
			
				 return;
			 }
			 n = n.getNext();
		 }
	 }
	 
	 
	 public static int addNewHeapNode(ClassInfo typeClassInfo, ThreadInfo ti, int daIndex, Object attr,
			 KernelState ks, PathCondition pcHeap, SymbolicInputHeap symInputHeap) {
		 daIndex = ks.da.newObject(typeClassInfo, ti);
		 String refChain = ((SymbolicInteger) attr).getName() + "[" + daIndex + "]"; // do we really need to add daIndex here?
		 SymbolicInteger newSymRef = new SymbolicInteger( refChain);
		 ElementInfo eiRef = DynamicArea.getHeap().get(daIndex);

		 // neha: this change allows all the fields in the class hierarchy of the
		 // object to be initialized as symbolic and not just its instance fields
		 Fields f = eiRef.getFields();
		 int numOfFields = f.getNumberOfFields();
		 FieldInfo[] fields = new FieldInfo[numOfFields];
		 for(int fieldIndex = 0; fieldIndex < numOfFields; fieldIndex++) {
			 fields[fieldIndex] = f.getFieldInfo(fieldIndex);
		 }

		 Helper.initializeInstanceFields(fields, eiRef,refChain);

		 //neha: this change allows all the static fields in the class hierarchy
		 // of the object to be initialized as symbolic and not just its immediate
		 // static fields
		 ClassInfo superClass = typeClassInfo;
		 while(superClass != null) {
			 FieldInfo[] staticFields = superClass.getDeclaredStaticFields();
			 Helper.initializeStaticFields(staticFields, superClass, ti);
			 superClass = superClass.getSuperClass();
		 }

		 // create new HeapNode based on above info
		 // update associated symbolic input heap
		 HeapNode n= new UberLazyHeapNode(daIndex,typeClassInfo,newSymRef,refChain);
		 symInputHeap._add(n);
		 pcHeap._addDet(Comparator.NE, newSymRef, new IntegerConstant(-1));
		 return daIndex;

	 }
	  
	  
}