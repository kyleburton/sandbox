package com.github.kyleburton.sandbox.quartz;


import clojure.lang.Namespace;
import clojure.lang.RT;
import clojure.lang.Symbol;
import clojure.lang.Var;

import org.quartz.Job;
import org.quartz.JobDetail;
import org.quartz.JobDataMap;
import org.quartz.JobExecutionContext;
import org.quartz.JobExecutionException;

/**
 * Quartz Job class for executing a clojure function with an array of
 * arguments.
 *
 * @author Kyle R. Burton <kyle.burton@gmail.com>
 */
public class ClojureJob implements Job {
  private static final Class CLASS = ClojureJob.class;

  public ClojureJob() {
  }

  public void execute(JobExecutionContext context) throws JobExecutionException {
    System.err.println(CLASS.getName() + ".execute: context = " + context);
    // look up the clojure
    JobDetail detail     = context.getJobDetail();
    JobDataMap dataMap   = detail.getJobDataMap();
    String namespaceName = dataMap.getString("job.clojure.namespace");
    String functionName  = dataMap.getString("job.clojure.function");
    // Object args[]        = dataMap.getString("job.clojure.arguments");
    Symbol symNamespace  = Symbol.create(namespaceName);
    Namespace namespace  = Namespace.findOrCreate(symNamespace);
    Var fn = Var.intern(namespace,Symbol.create(functionName));
    // fn.applyTo(args);
    try {
      fn.invoke();
    }
    catch(Exception ex) {
      throw new JobExecutionException(ex);
    }
  }
}