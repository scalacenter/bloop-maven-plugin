package scala_maven;

public class AppLauncher extends Launcher {
    public AppLauncher() {
        this.id = "";
        this.mainClass = "";
        this.jvmArgs = new String[0];
        this.args = new String[0];
    }

    public AppLauncher(String id, String mainClass, String[] jvmArgs, String[] args) {
        this.id = id;
        this.mainClass = mainClass;
        this.jvmArgs = jvmArgs;
        this.args = args;
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
