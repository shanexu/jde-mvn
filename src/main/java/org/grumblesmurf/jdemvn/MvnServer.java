package org.grumblesmurf.jdemvn;

import org.apache.maven.cli.CLIReportingUtils;
import org.apache.maven.embedder.Configuration;
import org.apache.maven.embedder.ConfigurationValidationResult;
import org.apache.maven.embedder.DefaultConfiguration;
import org.apache.maven.embedder.MavenEmbedder;
import org.apache.maven.embedder.MavenEmbedderConsoleLogger;
import org.apache.maven.embedder.MavenEmbedderException;
import org.apache.maven.embedder.MavenEmbedderLogger;
import org.apache.maven.errors.CoreErrorReporter;
import org.apache.maven.errors.DefaultCoreErrorReporter;
import org.apache.maven.execution.DefaultMavenExecutionRequest;
import org.apache.maven.execution.MavenExecutionRequest;

import java.util.Arrays;

import java.io.File;

public class MvnServer
{
    private static MvnServer me;

    public static MvnServer getInstance() {
        if (me == null) {
            me = new MvnServer();
            if (!me.isUsable())
                return null;
        }
        return me;
    }

    private Configuration configuration;
    private MavenEmbedder mavenEmbedder;
    private CoreErrorReporter errorReporter;
    private MavenEmbedderLogger logger;

    protected MvnServer() {
        configuration = buildEmbedderConfiguration();
        errorReporter = new DefaultCoreErrorReporter();
        logger = new MavenEmbedderConsoleLogger();
        
        if (validateConfiguration()) {
            try {
                mavenEmbedder = new MavenEmbedder(configuration);
            } catch (MavenEmbedderException e) {
                CLIReportingUtils.showError("Unable to start the embedder: ", e, false, errorReporter, logger);
            }
        }
    }

    public boolean isUsable() {
        return mavenEmbedder != null;
    }
            
    private Configuration buildEmbedderConfiguration() {
        Configuration configuration = new DefaultConfiguration()
            .setErrorReporter(errorReporter)
            .setUserSettingsFile(MavenEmbedder.DEFAULT_USER_SETTINGS_FILE)
            .setMavenEmbedderLogger(logger);
        return configuration;
    }

    private boolean validateConfiguration() {
        ConfigurationValidationResult cvr =
            MavenEmbedder.validateConfiguration(configuration);
        if (!cvr.isValid()) {
            if (cvr.getUserSettingsException() != null) { 
                CLIReportingUtils.showError("Error reading user settings: ",
                                            cvr.getUserSettingsException(),
                                            false,
                                            errorReporter,
                                            logger);
            }
            if (cvr.getGlobalSettingsException() != null) { 
                CLIReportingUtils.showError("Error reading global settings: ",
                                            cvr.getGlobalSettingsException(),
                                            false,
                                            errorReporter,
                                            logger);
            }
            return false;
        }
        return true;
    }
    
    public void run(String pomFileName, boolean recursive, String... goals) {
        File pomFile = new File(pomFileName);
        MavenExecutionRequest request = new DefaultMavenExecutionRequest()
            .setBaseDirectory(pomFile.getParentFile())
            .setGoals(Arrays.asList(goals))
            .setRecursive(recursive);
        
        mavenEmbedder.execute(request);
    }
}
