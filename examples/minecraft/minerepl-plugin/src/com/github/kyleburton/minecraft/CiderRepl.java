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

public class CiderRepl extends EZPlugin {
  private static boolean isRunning = false;
  private static Logman LOG = null;
  private static int port = 4444;

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
      if (isRunning) {
        LOG.info("CiderRepl.startCiderRepl: already running on port:" + port);
        return;
      }

      if (parameters.length > 1) {
        try {
          port = Integer.parseInt(parameters[1]);
        }
        catch (Exception ex) {
          LOG.info("Error parsing port from parameter: '" + parameters[1] + "'");
        }
      }

      LOG.info("CiderRepl.startCiderRepl: ok, start repl on port=" + port);
    }

  }
}

