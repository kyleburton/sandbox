package com.github.kyleburton;

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;

public class MusicLibraryTest extends TestCase {

    public MusicLibraryTest( String testName ) {
        super( testName );
    }

    public static Test suite() {
        return new TestSuite( MusicLibraryTest.class );
    }

    public void testMusicLibrary() {
        assertTrue( true );
    }
}
