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

import java.io.File;
import java.util.ArrayList;
import java.util.List;
import java.util.Properties;

import org.apache.maven.cli.MavenCli;

public enum MvnServer {
	INSTANCE;

	private MavenCli mavenCli;

	private MvnServer() {
		mavenCli = new MavenCli();
	}

	public boolean run(String pomFileName, String... goals) {
		return run(pomFileName, false, goals).run();
	}

	public RunDescriptor run(String pomFileName, boolean recursive,
			String... goals) {
		RunDescriptor run = new RunDescriptor();
		run.setPom(new File(pomFileName));
		run.setRecursive(recursive);
		run.setGoals(goals);
		return run;
	}

	public static class RunDescriptor {
		File pom;
		boolean recursive;
		String[] goals;
		Properties properties = new Properties();

		public void setPom(File pom) {
			this.pom = pom;
		}

		public void setRecursive(boolean recursive) {
			this.recursive = recursive;
		}

		public void setGoals(String[] goals) {
			this.goals = goals;
		}

		public RunDescriptor addProperty(String key, String value) {
			properties.put(key, value);
			return this;
		}

		public boolean run() {

			List<String> args = new ArrayList<String>(16);

			args.add("-f");
			args.add(pom.getPath());

			for (Object key : properties.keySet()) {
				args.add("-D" + key.toString() + "="
						+ properties.getProperty(key.toString()));
			}

			for (String goal : goals) {
				args.add(goal);
			}

			int result = INSTANCE.mavenCli.doMain(
					args.toArray(new String[args.size()]), pom.getParent(),
					null, null);
			System.out.println(result);
			return result == 0;
		}
	}

}
