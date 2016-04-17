package com.github.kyleburton;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import clojure.java.api.Clojure;
import clojure.lang.IFn;

public class Repl {
    public static final Logger LOG                       = LoggerFactory.getLogger(Repl.class.getName());
    public static final int    DEFAULT_REPL_PORT         = 4123;
    public static final String DEFAULT_REPL_BIND_ADDRESS = "127.0.0.1";

    public static Object server = null;

    public static int getReplPort() {
	// pull from System.properties if set
	if (null != System.getProperty("DEFAULT_REPL_PORT")) {
	    return Integer.parseInt(System.getProperty("DEFAULT_REPL_PORT"));
	}

	// pull from env var if set
	if (null != System.getenv("DEFAULT_REPL_PORT")) {
	    return Integer.parseInt(System.getenv("DEFAULT_REPL_PORT"));
	}

	// fallback to hard-coded default
	return DEFAULT_REPL_PORT;
    }


    public static String getBindAddress() {
	// pull from System.properties if set
	if (null != System.getProperty("DEFAULT_REPL_PORT")) {
	    return System.getProperty("DEFAULT_REPL_PORT");
	}

	// pull from env var if set
	if (null != System.getenv("DEFAULT_REPL_PORT")) {
	    return System.getenv("DEFAULT_REPL_PORT");
	}
	return "127.0.0.1";
    }

    public static boolean doLaunchCiderNRepl() {
	if (null != System.getProperty("NREPL_TYPE") 
	    && "cider".equals(System.getProperty("NREPL_TYPE") )) {
	    return true;
	}

	if (null != System.getenv("NREPL_TYPE") 
	    && "cider".equals(System.getenv("NREPL_TYPE") )) {
	    return true;
	}

	return true;
    }

    public static void cljRequire(String ns) {
	IFn require = Clojure.var("clojure.core","require");
	LOG.info(String.format("    ... clojure.core/require %s", ns));
	require.invoke(Clojure.read(ns));
    }

    public static Object cljEval(String code) {
	IFn eval = Clojure.var("clojure.core", "eval");
	return eval.invoke(Clojure.read(code));
    }

    public static void launchCiderNRepl(String bindAddress, int port) {
	LOG.info(String.format("    ... cider nrepl"));
	cljRequire("clojure.tools.nrepl.server");
	cljRequire("cider.nrepl");
	server = cljEval( "(clojure.tools.nrepl.server/start-server"
			  + "\n  :port " + port
			  + "\n  :bind \"" + bindAddress + "\""
			  + "\n  :handler cider.nrepl/cider-nrepl-handler)");
    }

    public static void launchCiderNRepl() {
	launchCiderNRepl(DEFAULT_REPL_BIND_ADDRESS, DEFAULT_REPL_PORT);
    }

    public static void launchNRepl(String bindAddress, int port) {
	LOG.info(String.format("    ... stanard nrepl"));
	cljRequire("clojure.tools.nrepl.server");
	server = cljEval( "(clojure.tools.nrepl.server/start-server"
			  + "\n  :port " + port
			  + "\n  :bind \"" + bindAddress + "\")");
    }

    public static void launchNRepl() {
	launchNRepl(DEFAULT_REPL_BIND_ADDRESS, DEFAULT_REPL_PORT);
    }

    public static void main (String [] args) {
	LOG.info(String.format("Launching repl on %s:%s", getBindAddress(), getReplPort()));
	if (doLaunchCiderNRepl()) {
	    launchCiderNRepl(getBindAddress(), getReplPort());
	    LOG.info(String.format("server listening on background thread: %s", server));
	    return;
	}

	launchNRepl(getBindAddress(), getReplPort());
	LOG.info(String.format("server listening on background thread"));
	return;
    }
}
