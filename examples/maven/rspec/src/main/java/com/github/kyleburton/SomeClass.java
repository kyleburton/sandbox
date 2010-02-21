
package com.github.kyleburton;

import org.apache.commons.io.IOUtils;

import java.net.*;
import java.io.*;

public class SomeClass {
  private String _userName;
  private String _remoteUrl;
  private String _theContent = null;
  private Downloader _downloader;

  public SomeClass() {
    _userName = "*a default*";
    _remoteUrl = "http://localhost/";
    _downloader = new Downloader(_remoteUrl);
  }

  public SomeClass(String name, String url) {
    _userName = name;
    _remoteUrl = url;
    _downloader = new Downloader(url);
  }

  public String getContent() throws IOException {
    if ( null == _theContent )
      _theContent = _downloader.download();

    return _theContent;
  }

  public String getUserName() {
    return _userName;
  }

  public void setUserName(String userName) {
    _userName = userName;
  }

  public String getRemoteUrl() {
    return _remoteUrl;
  }

  public void setRemoteUrl(String remoteUrl) {
    _remoteUrl = remoteUrl;
    _downloader = new Downloader(_remoteUrl);
  }

  public void setDownloader(Downloader downloader) {
    _downloader = downloader;
  }

  public static class Downloader {

    private String url;
    public Downloader(String url) {
      this.url = url;
    }

    public String download() throws IOException {
      return IOUtils.toString(new URL(url).openStream());
    }
  }

}
