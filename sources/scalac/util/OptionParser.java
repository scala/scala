/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scalac.util;

import scala.tools.util.Position;

import java.text.Format;
import java.text.MessageFormat;
import java.util.List;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.StringTokenizer;

import scalac.PhaseDescriptor;
//import scalac.optimizer.OptimizePhase;

public class CommandParser {

    private final String product;
    private final String version;
    private final String syntax;
    private final Reporter reporter;
    private final List/*<ArgumentParser>*/ parsers;

    public CommandParser(String product, String version, String syntax,
        Reporter reporter)
    {
        this.product = product;
        this.version = version;
        this.syntax = syntax;
        this.reporter = reporter;
        this.parsers = new ArrayList();
    }

    public String product() {
        return product;
    }

    public String version() {
        return version;
    }

    public String syntax() {
        return syntax;
    }

    public Reporter reporter() {
        return reporter;
    }

    public boolean add(ArgumentParser parser) {
        return parsers.add(parser);
    }

    public void add(int index, ArgumentParser parser) {
        parsers.add(index, parser);
    }

    public boolean remove(ArgumentParser parser) {
        return parsers.remove(parser);
    }

    public List parsers() {
        return parsers;
    }

    public boolean parse(String[] args) {
        int errors = reporter.errors();
        for (int i = 0; i < args.length; ) {
            for (int j = 0; j < parsers.size(); j++) {
                ArgumentParser parser = (ArgumentParser)parsers.get(j);
                if (parser.matches(args, i)) {
                    i = parser.consume(args, i);
                    break;
                }
	    }
	}
        return reporter.errors() == errors;
    }

    public String getHelpMessage() {
        Format format = new MessageFormat("  {0}\t  {1}");
        List options = new ArrayList(parsers.size());
        for (int i = 0; i < parsers.size(); i++) {
            if (!(parsers.get(i) instanceof OptionParser)) continue;
            OptionParser parser = (OptionParser)parsers.get(i);
            String option = parser.getHelpMessage(format);
            if (option != null) options.add(option);
        }
        StringBuffer buffer = new StringBuffer();
        buffer.append("usage: ").append(product());
        if (options.size() > 0) buffer.append(" <options>");
        if (syntax != null) buffer.append(' ').append(syntax);
        buffer.append(Strings.EOL);
        if (options.size() > 0) {
            buffer.append("where possible options include:");
            buffer.append(Strings.EOL);
            buffer.append(Strings.format(options));
        }
        return buffer.toString();
    }

    public void error(String message) {
        reporter.error(new Position(product), message);
    }

    public void warning(String message) {
        reporter.warning(new Position(product), message);
    }
}

public abstract class ArgumentParser {

    public final CommandParser command;

    public ArgumentParser(CommandParser command) {
        this.command = command;
    }

    public abstract boolean matches(String[] args, int index);
    public abstract int consume(String[] args, int index);

}

public class UnknownArgumentParser extends ArgumentParser {

    public UnknownArgumentParser(CommandParser command) {
        super(command);
    }

    public boolean matches(String[] args, int index) {
        return true;
    }

    public int consume(String[] args, int index) {
        command.error("don't known what to do with '" + args[index] + "'");
        return index + 1;
    }
}

public class ScalaFileArgumentParser extends ArgumentParser {

    public final List list;

    public ScalaFileArgumentParser(CommandParser command) {
        super(command);
        this.list = new ArrayList();
    }

    public boolean matches(String[] args, int index) {
        return args[index].endsWith(".scala");
    }

    public int consume(String[] args, int index) {
        list.add(args[index]);
        return index + 1;
    }

    public String[] toArray() {
        return (String[])list.toArray(new String[list.size()]);
    }
}

public class ScalaProgramArgumentParser extends ArgumentParser {

    public String main;
    public String[] args;

    public ScalaProgramArgumentParser(CommandParser command) {
        super(command);
    }

    public boolean matches(String[] args, int index) {
        return args[index].equals("--");
    }

    public int consume(String[] args, int index) {
        if (index + 1 < args.length) {
            this.main = args[index + 1];
            this.args = new String[args.length - index - 2];
            System.arraycopy(args, index + 2, this.args, 0, this.args.length);
            return args.length;
        } else {
            command.error("option --: missing module name");
            return args.length;
        }
    }
}

public abstract class OptionParser extends ArgumentParser {

    public final String option;
    public final String description;

    public OptionParser(CommandParser command, String option,
        String description)
    {
        super(command);
        this.option = option;
        this.description = description;
    }

    public String getHelpSyntax() {
        return "-" + option;
    }

    public String getHelpDescription() {
        return description;
    }

    public void getHelpMessageArgs(List args) {
        args.add(getHelpSyntax());
        args.add(getHelpDescription());
    }

    public String getHelpMessage(Format format) {
        if (description == null) return null;
        List args = new ArrayList();
        getHelpMessageArgs(args);
        return format.format(args.toArray());
    }

    public void error(String message) {
        command.error("option -" + option + ": " + message);
    }

    public void warning(String message) {
        command.warning("option -" + option + ": " + message);
    }
}
/*
public class OptimizeOptionParser extends OptionParser {

    private final OptimizePhase optimizer;
    public boolean optimize;

    public OptimizeOptionParser(CommandParser command,
        String option, String description, OptimizePhase optimizer)
    {
        super(command, option, description);
        this.optimizer = optimizer;
        this.optimize = false;
    }

    public boolean matches(String[] args, int index) {
        return args[index].equals("-" + option);
    }

    public int consume(String[] args, int index) {
        optimizer.setOptions(args[index].substring(1 + option.length()));
        optimize = true;
        return index + 1;
    }

    public String getHelpSyntax() {
        return super.getHelpSyntax() + "[:<options>]";
    }
}
*/
public class VersionOptionParser extends OptionParser {

    private final String version;

    public VersionOptionParser(CommandParser command,
        String option, String description, String version)
    {
        super(command, option, description);
        this.version = version;
    }

    public boolean matches(String[] args, int index) {
        return args[index].equals("-" + option);
    }

    public int consume(String[] args, int index) {
        System.out.println(version);
        System.exit(0);
        return index + 1;
    }
}

public class HelpOptionParser extends OptionParser {

    public HelpOptionParser(CommandParser command,
        String option, String description)
    {
        super(command, option, description);
    }

    public boolean matches(String[] args, int index) {
        return args[index].equals("-?") ||
            args[index].equals("-" + option) ||
            args[index].equals("--" + option);
    }

    public int consume(String[] args, int index) {
        System.out.println(command.getHelpMessage());
        System.exit(0);
        return index + 1;
    }

    public String getHelpSyntax() {
        return "-? " + super.getHelpSyntax();
    }
}

public class UnknownOptionParser extends OptionParser {

    public UnknownOptionParser(CommandParser command) {
        super(command, "", null);
    }

    public boolean matches(String[] args, int index) {
        return args[index].startsWith("-");
    }

    public int consume(String[] args, int index) {
        command.error("unknown option " + args[index]);
        return index + 1;
    }
}

public class BooleanOptionParser extends OptionParser {

    public boolean value;

    public BooleanOptionParser(CommandParser command,
        String option, String description, boolean value)
    {
        super(command, option, description);
        this.value = value;
    }

    public boolean matches(String[] args, int index) {
        return args[index].equals("-" + option);
    }

    public int consume(String[] args, int index) {
        value = true;
        return index + 1;
    }
}

public class StringOptionParser extends OptionParser {

    public String value;
    public String argument;

    public StringOptionParser(CommandParser command,
        String option, String description, String argument, String value)
    {
        super(command, option, description);
        this.argument = argument;
        this.value = value;
    }

    public boolean matches(String[] args, int index) {
        return args[index].equals("-" + option);
    }

    public int consume(String[] args, int index) {
        if (index + 1 < args.length) {
            value = args[index + 1];
            return index + 2;
        } else {
            error("missing argument");
            return index + 1;
        }
    }

    public String getHelpSyntax() {
        String syntax = super.getHelpSyntax();
        if (argument != null) syntax = syntax + " <" + argument + ">";
        return syntax;
    }
}

public class PhaseSetOptionParser extends OptionParser {

    private final PhaseDescriptor[] phases;
    private final int flag;
    private final PrefixMatcher matcher;

    public PhaseSetOptionParser(CommandParser command,
        String option, String description, PhaseDescriptor[] phases, int flag)
    {
        super(command, option, description);
        this.phases = phases;
        this.flag = flag;
        this.matcher = new PrefixMatcher();
        for (int i = 0; i < phases.length; i++) {
            PhaseDescriptor phase = phases[i];
            matcher.insert(phase.name(), phase, phase.description());
        }
    }

    public boolean matches(String[] args, int index) {
        return args[index].startsWith("-" + option + ":");
    }

    public int consume(String[] args, int index) {
        StringTokenizer tokens = new StringTokenizer(
            args[index].substring(option.length() + 2), ",");
        while (tokens.hasMoreTokens()) consumePhase(tokens.nextToken());
        return index + 1;
    }

    public void consumePhase(String token) {
        if (token.equals("all")) {
            for (int i = 0; i < phases.length; i++)
                phases[i].addFlag(flag, false);
            return;
        }
        PhaseDescriptor phase = lookup(getPhaseName(token));
        if (phase != null) {
            boolean before = getBeforeFlag(token);
            boolean after = getAfterFlag(token) || !before;
            if (before) phase.addFlag(flag, true);
            if (after) phase.addFlag(flag, false);
        }
    }

    public PhaseDescriptor lookup(String name) {
        if (name.length() == 0) {
            error("illegal zero-length phase name");
            return null;
        }
        PrefixMatcher.Entry[] entries = matcher.lookup(name);
        if (entries.length == 1) return (PhaseDescriptor)entries[0].value;
        error(matcher.getErrorMessage(name, entries, "phase name"));
        return null;
    }

    public boolean getBeforeFlag(String token) {
        for (int i = token.length(); 0 < i--; ) {
            switch (token.charAt(i)) {
            case '-': return true;
            case '+': continue;
            default : return false;
            }
        }
        return false;
    }

    public boolean getAfterFlag(String token) {
        for (int i = token.length(); 0 < i--; ) {
            switch (token.charAt(i)) {
            case '-': continue;
            case '+': return true;
            default : return false;
            }
        }
        return false;
    }

    public String getPhaseName(String token) {
        for (int i = token.length(); 0 < i--; ) {
            switch (token.charAt(i)) {
            case '-': continue;
            case '+': continue;
            default : return token.substring(0, i + 1);
            }
        }
        return "";
    }

    public String getHelpSyntax() {
        return super.getHelpSyntax() + ":<phases>";
    }
}

public class PrintOptionParser extends PhaseSetOptionParser {

    public boolean tokens;

    public PrintOptionParser(CommandParser command,
        String option, String description, PhaseDescriptor[] phases, int flag)
    {
        super(command, option, description, phases, flag);
        this.tokens = false;
    }

    public void consumePhase(String token) {
        if ("tokens".equals(token))
            tokens = true;
        else
            super.consumePhase(token);
    }

}

public class ChoiceOptionParser extends OptionParser {

    public final String argument;
    public final String[] choices;

    public String value;

    public ChoiceOptionParser(CommandParser command,
        String option, String description, String argument, String[] choices,
        String value)
    {
        super(command, option, description);
        this.argument = argument;
        this.choices = choices;
        this.value = value;
    }

    public boolean matches(String[] args, int index) {
        return args[index].startsWith("-" + option + ":");
    }

    public int consume(String[] args, int index) {
        String choice = args[index].substring(option.length() + 2);
        boolean found = false;
        for (int i = 0; i < choices.length; i++) {
            if (choices[i].equals(choice)) { found = true; break; }
        }
        if (found) {
            value = choice;
        } else if (choice.length() > 0) {
            error("unknown " + argument + " '" + choice + "'");
        } else {
            error("missing " + argument);
        }
        return index + 1;
    }

    public String getHelpSyntax() {
        String syntax = super.getHelpSyntax();
        if (argument != null) syntax = syntax + ":<" + argument + ">";
        return syntax;
    }
}
