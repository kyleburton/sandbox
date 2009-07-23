package com.github.kyleburton.sandbox.quartz;

import clojure.lang.Namespace;
import clojure.lang.RT;
import clojure.lang.Symbol;
import clojure.lang.Var;
import clojure.lang.IFn;

import org.quartz.Job;
import org.quartz.JobDetail;
import org.quartz.JobDataMap;
import org.quartz.JobExecutionContext;
import org.quartz.JobExecutionException;

/**
 * Quartz Job class for executing a clojure function.  The clojure
 * function will take a single argument, the JobExecutionContext which
 * is passed to this Job's execute method.
 *
 * NB: There is no way to capture or propigate an error from the
 * called function back to the context where the job was scheduled.
 *
 * http://www.opensymphony.com/quartz/
 *
 * @author Kyle R. Burton <kyle.burton@gmail.com>
 */
public class ClojureJob implements Job {
  /** JobExecutionContext/JobDetail/JobDataMap Parameter for the namespace of the function that will be called. */
  public  static final String NAMESPACE_PARAMETER     = "job.clojure.namespace";
  /** JobExecutionContext/JobDetail/JobDataMap Parameter for the name of the function that will be called. */
  public  static final String FUNCTION_NAME_PARAMETER = "job.clojure.function";
  /** */
  public  static final String FUNCTION_PARAMETER = "job.clojure.fn";
  private static final Class CLASS = ClojureJob.class;

  /**
   * Quartz required no-arg constructor.  Does nothing.
   */
  public ClojureJob() {
  }

  /**
   * Execute implementation, look up the clojure function specified in
   * the JobDataMap, invoke it with the JobExecutionContext.
   * @param context the JobExecutionContext passed in by quartz
   * @throws JobExecutionContext if the function can not be looked up,
   * or if the function throws an exception.
   */
  // @Override
  public void execute(JobExecutionContext context) throws JobExecutionException {
    if ( null != contextParameter(context,FUNCTION_PARAMETER) ) {
      executeFunction(context,(IFn)contextParameter(context,FUNCTION_PARAMETER));
      return;
    }

    executeVar(context);
  }

  private void executeVar( JobExecutionContext context) throws JobExecutionException {
    Var fn = lookupClojureFunction(context);
    if ( null == fn ) {
      throw new JobExecutionException(
        String.format(CLASS.getName() + ".execute: unable to find the specified function, namespace=%s; function=%s", 
                      contextParameterString(context,NAMESPACE_PARAMETER),
                      contextParameterString(context,FUNCTION_NAME_PARAMETER)));
    }

    try {
      fn.invoke(context);
    }
    catch(Exception ex) {
      throw new JobExecutionException(ex);
    }
  }

  private void executeFunction( JobExecutionContext context, IFn fn) throws JobExecutionException {
    try {
      fn.invoke(context);
    }
    catch(Exception ex) {
      throw new JobExecutionException(ex);
    }
  }

  /**
   * Helper function for pulling parameters from the JobDataMap
   *
   * @param JobExecutionException the context to pull the parameter from
   * @param name the parameter to pull from the JobDataMap
   * @return the string value from the JobDataMap
   */
  private String contextParameterString(JobExecutionContext context, String name) {
    return context.getJobDetail().getJobDataMap().getString(name);
  }

  private Object contextParameter(JobExecutionContext context, String name) {
    return context.getJobDetail().getJobDataMap().get(name);
  }

  /**
   * Helper function for looking up the clojure function specified in
   * the quartz job.
   *
   * @param JobExecutionException the context passed in from quartz
   * @return the clojure Var which is the function
   */
  private Var lookupClojureFunction(JobExecutionContext context) { 
    String namespaceName = contextParameterString(context,NAMESPACE_PARAMETER);
    String functionName  = contextParameterString(context,FUNCTION_NAME_PARAMETER);
    Symbol symNamespace  = Symbol.create(namespaceName);
    Namespace namespace  = Namespace.findOrCreate(symNamespace);
    return Var.intern(namespace,Symbol.create(functionName));
  }
}