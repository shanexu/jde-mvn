/**
 * Copyright (c) 2008 Espen Wiborg <espenhw@grumblesmurf.org>
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */
package org.grumblesmurf.jdemvn;

import org.junit.Test;
import org.junit.Ignore;
import static org.junit.Assert.*;

public class MvnServerTest
{
    @Test
    public void testMNG3838() throws Exception {
        assertTrue(MvnServer.INSTANCE.run(System.getProperty("basedir") + "/src/test/resources/mng3838/pom.xml", "help:effective-pom"));
    }
}
