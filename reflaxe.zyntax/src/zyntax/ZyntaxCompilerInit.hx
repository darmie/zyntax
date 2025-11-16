package zyntax;

#if (macro || zyntax_runtime)

import reflaxe.ReflectCompiler;
import reflaxe.preprocessors.ExpressionPreprocessor;

/**
 * Initialization for Zyntax backend
 */
class ZyntaxCompilerInit {
    public static function Start() {
        #if !eval
        Sys.println("ZyntaxCompilerInit.Start can only be called from a macro context.");
        return;
        #end

        #if (haxe_ver < "4.3.0")
        Sys.println("Reflaxe/Zyntax requires Haxe version 4.3.0 or greater.");
        return;
        #end

        ReflectCompiler.AddCompiler(new ZyntaxCompiler(), {
            expressionPreprocessors: [
                // Transform trace() to $String$println() FIRST (before other transformations)
                Custom(new TraceTransformer()),
                // Transform string concatenation to runtime calls
                Custom(new StringOperatorTransformer()),
                SanitizeEverythingIsExpression({}),
                PreventRepeatVariables({}),
                RemoveSingleExpressionBlocks,
                RemoveConstantBoolIfs,
                RemoveUnnecessaryBlocks,
                RemoveReassignedVariableDeclarations,
                RemoveLocalVariableAliases,
                MarkUnusedVariables,
            ],
            fileOutputExtension: ".zbc",
            outputDirDefineName: "zyntax-output",
            fileOutputType: FilePerClass,
            reservedVarNames: reservedNames(),
            targetCodeInjectionName: "__ZYNTAX__",
            trackUsedTypes: true,
            ignoreBodilessFunctions: false  // Include extern functions
        });
    }

    static function reservedNames() {
        return [
            "as", "async", "await", "break", "const", "continue", "crate",
            "dyn", "else", "enum", "extern", "false", "fn", "for", "if",
            "impl", "in", "let", "loop", "match", "mod", "move", "mut",
            "pub", "ref", "return", "self", "Self", "static", "struct",
            "super", "trait", "true", "type", "unsafe", "use", "where",
            "while", "abstract", "become", "box", "do", "final", "macro",
            "override", "priv", "typeof", "unsized", "virtual", "yield"
        ];
    }
}

#end
