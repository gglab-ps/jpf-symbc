package gov.nasa.jpf.symbc.string.graph;

import gov.nasa.jpf.symbc.string.SymbolicIndexOf2Integer;

import java.util.ArrayList;
import java.util.List;

public class EdgeIndexOf2 implements Edge{
	Vertex source, dest;
	SymbolicIndexOf2Integer sioi;
	String name;
	
	public EdgeIndexOf2 (String name, Vertex source, Vertex dest, SymbolicIndexOf2Integer sioi) {
		this.name = name;
		this.source = source;
		this.dest = dest;
		this.sioi = sioi;
	}
	
	@Override
	public boolean allVertecisAreConstant() {
		return source.isConstant() && dest.isConstant();
	}

	@Override
	public Vertex getDest() {
		return dest;
	}

	@Override
	public String getName() {
		return name;
	}

	@Override
	public Vertex getSource() {
		return source;
	}

	@Override
	public List<Vertex> getSources() {
		List<Vertex> result = new ArrayList<Vertex>();
		result.add (source);
		return result;
	}

	@Override
	public boolean isDirected() {
		return true;
	}

	@Override
	public boolean isHyper() {
		return false;
	}

	@Override
	public void setDest(Vertex v) {
		this.dest = v;
	}

	@Override
	public void setSource(Vertex v) {
		this.source= v;
	}
	
	public SymbolicIndexOf2Integer getIndex () {
		return sioi;
	}
}
