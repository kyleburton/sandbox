package com.github.kyleburton.logback;

import ch.qos.logback.core.LogbackException;
import ch.qos.logback.core.AppenderBase;

import ch.qos.logback.classic.spi.ILoggingEvent;

import java.net.InetAddress;
import java.net.UnknownHostException;
import java.io.IOException;
import java.util.List;
import java.util.concurrent.atomic.AtomicLong;

import com.aphyr.riemann.client.RiemannClient;
import com.aphyr.riemann.client.UdpTransport;
import com.aphyr.riemann.client.EventDSL;
import com.aphyr.riemann.client.SynchronousTransport;;
import com.aphyr.riemann.Proto.Msg;
import com.aphyr.riemann.Proto.Event;

public class RiemannAppender<E> extends AppenderBase<E> {
  private static final String DEFAULT_PORT = "5555";
  private static final String DEFAULT_HOST = "localhost";

  private String serviceName     = "*no-service-name*";
  private String riemannHostname = DEFAULT_HOST;
  private String riemannPort     = DEFAULT_PORT;
  private String hostname        = "*no-host-name*";
  private int    maxBufferSize   = 16384;

  public static AtomicLong timesCalled = new AtomicLong(0);

  private static final boolean debug = false;

  private RiemannClient riemannClient = null;

  public void start() {
    try {
      if (debug) {
        System.err.println(String.format("%s.start()", this));
      }
      SynchronousTransport transport = new SimpleUdpTransport(riemannHostname, Integer.parseInt(riemannPort));
      final RiemannClient cli = new RiemannClient(transport);
      riemannClient = cli;
      riemannClient.connect();
    }
    catch (IOException ex) {
      if (debug) {
        System.err.println(String.format("%s: Error initializing: %s", this, ex));
      }
      throw new RuntimeException(ex);
    }
    super.start();
  }

  public void stop() {
    if (debug) {
      System.err.println(String.format("%s.stop()", this));
    }
    super.stop();
  }

  public String toString () {
    return String.format(
        "RiemannAppender{hashCode=%s;serviceName=%s;riemannHostname=%s;riemannPort=%s;hostname=%s}",
        hashCode(),
        serviceName,
        riemannHostname,
        riemannPort,
        hostname);
  }

  protected synchronized void append(E event) /* throws LogbackException */ {
    timesCalled.incrementAndGet();
    ILoggingEvent logEvent = (ILoggingEvent) event;
    EventDSL rEvent;
    try {
      rEvent = riemannClient.event().
        service(serviceName).
        host(hostname).
        state("error").
        attribute("message", logEvent.getFormattedMessage());

      if (logEvent.hasCallerData()) {
        StringBuilder sb = new StringBuilder();
        for ( StackTraceElement elt : logEvent.getCallerData()) {
          sb.append(elt);
          sb.append("\n");
        }
        rEvent.attribute("stacktrace", sb.toString());
      }

      try {
        rEvent.send();
        if (debug) {
          System.err.println(String.format("%s.append(logEvent): sent to riemann %s:%s", this, riemannHostname, riemannPort));
        }
      }
      catch (Exception ex) {
        if (debug) {
          System.err.println(String.format(
                "%s: Error sending event %s",
                this,
                ex));
          ex.printStackTrace();
        }

        riemannClient.reconnect();
        if (null != rEvent) {
          rEvent.send();
        }
      }
    }
    catch (Exception ex) {
      // do nothing
      if (debug) {
        System.err.println(String.format("%s: Error during append(): %s", ex));
        ex.printStackTrace();
      }
    }

    //System.err.println(String.format(
    //      "RiemannAppender{serviceName=%s;riemannHostname=%s;riemannPort=%s;hostname=%s}",
    //      serviceName,
    //      riemannHostname,
    //      riemannPort,
    //      hostname));

    //System.err.println("RiemannAppender: event: " + event);
    //System.err.println("RiemannAppender: event.getClass(): " + event.getClass());
  }


  public void setServiceName (String s) {
    serviceName = s;
  }

  public void setRiemannHostname (String s) {
    riemannHostname = s;
  }

  public void setRiemannPort (String s) {
    riemannPort = s;
  }

  public void setHostname (String s) {
    hostname = s;
  }
}
