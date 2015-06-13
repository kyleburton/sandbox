package com.github.kyleburton.minecraft;

import net.canarymod.plugin.Plugin;
import net.canarymod.logger.Logman;
import net.canarymod.Canary;
import net.canarymod.commandsys.*;
import net.canarymod.chat.MessageReceiver;
import net.canarymod.api.entity.living.humanoid.Player;
import net.canarymod.api.world.position.Location;
import net.canarymod.api.world.blocks.BlockType;
import com.pragprog.ahmine.ez.EZPlugin;

import clojure.lang.Symbol;
import clojure.lang.Var;
import clojure.lang.RT;
import clojure.java.api.Clojure;
import clojure.lang.IFn;

public class CiderRepl extends EZPlugin {
  public static boolean isRunning = false;
  private static Logman LOG = null;
  public static int port = 4444;

  public static Var REQUIRE = null;
  public static Object nreplServer = null;

  public CiderRepl () {
    super();
    CiderRepl.LOG = getLogman();
  }

  //@Override
  //public void enable () {
  //  LOG.info("CiderRepl.enable");
  //}

  //@Override
  //public void disable () {
  //  LOG.info("CiderRepl.disable");
  //}

  @Command(aliases = { "cider", "nrepl", "cider-nrepl" },
            description = "Start a Cider NRepl (Clojure)",
            permissions = { "" },
            toolTip = "/cider [port=4444]")
  public void startCiderRepl(MessageReceiver caller, String[] parameters) {
    synchronized(CiderRepl.class) {

      if (parameters.length > 1) {
        if ( "stop".equals(parameters[1]) && isRunning) {
          LOG.info("TODO: stop the server");
          // isRunning = false;
          return;
        }

        try {
          port = Integer.parseInt(parameters[1]);
        }
        catch (Exception ex) {
          LOG.info("Error parsing port from parameter: '" + parameters[1] + "', will fall back to default port=" + port);
        }
      }

      if (isRunning) {
        LOG.info("CiderRepl.startCiderRepl: already running on port:" + port);
        return;
      }

      LOG.info("CiderRepl.startCiderRepl: ok, start repl on port=" + port);

      IFn requireFn = Clojure.var("clojure.core", "require");
      IFn keywordFn = Clojure.var("clojure.core", "keyword");
      requireFn.invoke(Symbol.intern("clojure.tools.nrepl.server"));
      requireFn.invoke(Symbol.intern("cider.nrepl"));
      IFn startServerFn = Clojure.var("clojure.tools.nrepl.server", "start-server");
      IFn ciderNreplHandlerFn = Clojure.var("cider.nrepl", "cider-nrepl-handler");
      nreplServer = startServerFn.invoke(
          keywordFn.invoke("port"),
          port,
          keywordFn.invoke("handler"),
          ciderNreplHandlerFn
      );
      isRunning = true;
      LOG.info("CiderRepl.startCiderRepl: ok, started, server=" + nreplServer);
    }

  }
}

