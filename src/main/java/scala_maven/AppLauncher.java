package scala_maven;

public class AppLauncher extends Launcher {
    public AppLauncher() {
        this.id = "";
        this.mainClass = "";
        this.jvmArgs = new String[0];
        this.args = new String[0];
    }

    public String getId() {
        return id;
    }

    public String getMainClass() {
        return mainClass;
    }

    public String[] getJvmArgs() {
        return jvmArgs;
    }

    public String[] getArgs() {
        return args;
    }
}
