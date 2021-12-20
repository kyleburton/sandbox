package com.github.kyleburton.teporingo;

public class BreakerOpenException extends Exception {
  public BreakerOpenException(String msg) {
    super(msg);
  }

  public BreakerOpenException(String msg,Throwable cause) {
    super(msg,cause);
  }
}
