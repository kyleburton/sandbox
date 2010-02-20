
package com.github.kyleburton;

import org.apache.commons.io.IOUtils;

import java.net.*;
import java.io.*;

/**
 * This is an example Java class to show how to use RSpec to test a Java class.
 *
 * @author Kyle R. Burton <kyle.burton@gmail.com>
 */
public class SomeClass {
    private String _userName;
    private String _remoteUrl;
    private String _theContent = null;

    public SomeClass() {
        _userName = "*a default*";
        _remoteUrl = "http://localhost/";
    }

    public SomeClass(String name, String url) {
        _userName = name;
        _remoteUrl = url;
    }

    public String getContent() throws IOException {
        if ( null == _theContent )
            _theContent = downloadTheContent();

        return _theContent;
    }

    private String downloadTheContent() throws IOException {
       return IOUtils.toString(new URL(_remoteUrl).openStream());
    }

    public String getUserName() {
        return _userName;
    }

    public void setUserName(String _userName) {
        this._userName = _userName;
    }

    public String getRemoteUrl() {
        return _remoteUrl;
    }

    public void setRemoteUrl(String _remoteUrl) {
        this._remoteUrl = _remoteUrl;
    }

}
