import SwiftCompilerPlugin
import SwiftSyntax
import SwiftSyntaxBuilder
import SwiftSyntaxMacros

public struct BuilderMacro: MemberMacro {
    enum Error: Swift.Error {
        case wrongDeclarationSyntax
    }

    public static func expansion<
        Declaration: DeclGroupSyntax, Context: MacroExpansionContext
    >(
        of node: AttributeSyntax,
        providingMembersOf declaration: Declaration,
        in context: Context
    ) throws -> [DeclSyntax] {
        guard declaration.isStruct else {
            guard let diagnostic = Diagnostics.diagnose(declaration: declaration) else {
                throw Error.wrongDeclarationSyntax
            }

            context.diagnose(diagnostic)
            return []
        }

        let bodyGenerator = BuilderBodyGenerator()
        return try bodyGenerator.generateBody(from: declaration)
    }
}


@main
struct BuilderMacroPlugin: CompilerPlugin {
    let providingMacros: [Macro.Type] = [
        BuilderMacro.self,
        ThrowingBuilderMacro.self,
        FluentBuilderMacro.self
    ]
}

extension BuilderMacro.Error: CustomStringConvertible {
    var description: String {
        switch self {
        case .wrongDeclarationSyntax:
            return "Builder Macro supports only structs"
        }
    }
}
