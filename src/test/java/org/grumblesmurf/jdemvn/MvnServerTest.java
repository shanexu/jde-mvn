package org.grumblesmurf.jdemvn;

import org.junit.Test;
import org.junit.Ignore;
import static org.junit.Assert.*;

public class MvnServerTest
{
    @Test
    @Ignore("Waiting for resolution of MNG3838")
    public void testMNG3838() throws Exception {
        assertTrue(MvnServer.INSTANCE.run(System.getProperty("basedir") + "/src/test/resources/mng3838/pom.xml"));
    }
}
