CM.make("$/basis.cm");
CM.make("$/ml-yacc-lib.cm");
use "while_ast.sml";
use "WhileLang.yacc.sig";
use "WhileLang.yacc.sml";
use "WhileLang.lex.sml";
use "load-while.sml";
use "machine.sml";
Control.Print.printLength := 100000; (* set printing parameters so that *)
Control.Print.printDepth := 1000000; (* we’ll see all details *)
Control.Print.stringDepth := 1000000; (* and strings *)