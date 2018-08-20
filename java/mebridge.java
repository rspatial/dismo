// Author: Steven Phillips and Robert J. Hijmans
// Date: December 2009
// Version 0.1
// Licence GPL v3


import density.*;

public class mebridge {

	public static void main(String args[]) {
	
	}
	
	public String meversion (String str) {
		Utils utils = new Utils();
		str = Utils.getVersion();
		return str;
	}

	public String fit(String cmd[]) {
		Params params = new Params();
		String str = params.readFromArgs(cmd);
		if (str == null) {
			params.setSelections();
			Runner runner = new Runner(params);
			runner.start();
			runner.end();
		}	
		return(str);
	}
	
	public String fit(String cmd[], String[] categorical) {
		Params params = new Params();
		String str = params.readFromArgs(cmd);
		if (str == null) {
			params.setPrefixes(false);
			for (int i=0; i<categorical.length; i++) {
				if (categorical[i] != "") {
					params.parseParam("togglelayertype="+categorical[i]);
				}
			}
			params.setSelections();
			Runner runner = new Runner(params);
			runner.start();
			runner.end();
		}
		return(str);
	}

	
	public String testPredictArgs(String lambda, String[] args) {
		Evaluate eval=null;
		String par=null;
		try {

			eval = new Evaluate(lambda);
			Params params = eval.getParams();	
			par = params.readFromArgs(args);
			
		} catch (java.io.IOException e) {}
		
		return(par);

	}
	
	public double predict (String lambda, String[] vars, double[] vals, String[] args) {
		Evaluate eval=null;
		try {
			eval = new Evaluate(lambda);
			Params params = eval.getParams();	
			params.readFromArgs(args);
			
		} catch (java.io.IOException e) {}
		for (int i=0; i<vars.length; i++)
			eval.setValue(vars[i], vals[i]);
		return( eval.evaluate() );
	}


	public double[] predict (String lambda, String[] vars, double[][] vals, String[] args) {
		Evaluate eval=null;
		try {

			eval = new Evaluate(lambda);
			Params params = eval.getParams();	
			params.readFromArgs(args);
		
		} catch (java.io.IOException e) {}

		double[] p = new double[vals.length];
		for (int i=0; i<p.length; i++) {
			for (int j=0; j<vars.length; j++) {
				eval.setValue(vars[j], vals[i][j]);
				try {
					p[i] = eval.evaluate();
				} catch (Exception e) {
					p[i] = -9999;
				}
			}
		}
		return(p);
	}

	
	
}


//	public double predict (String lambda, String[] vars, int[] vals, String[] args) {
//		Evaluate eval=null;
//		try {
//			eval = new Evaluate(lambda);
//		} catch (java.io.IOException e) {}
//		for (int i=0; i<vars.length; i++)
//			eval.setValue(vars[i], vals[i]);
//		return( eval.evaluate() );
//	}
	


//	public double[] predict (String lambda, String[] vars, int[][] vals, String[] args) {
//		Evaluate eval=null;
//		try {
//			eval = new Evaluate(lambda);
//		} catch (java.io.IOException e) {}
//		double[] p = new double[vals.length];
//		for (int i=0; i<p.length; i++) {
//			for (int j=0; j<vars.length; j++) {
//				eval.setValue(vars[j], vals[i][j]);
//				try {
//					p[i] = eval.evaluate();
//				} catch (Exception e) {
//					p[i] = -9999;
//				}
//			}
//		}
//		return(p);
//	}

