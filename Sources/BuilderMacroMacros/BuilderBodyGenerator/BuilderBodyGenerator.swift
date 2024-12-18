//
//  BuilderBodyGenerator.swift
//  
//
//  Created by Piotr Szadkowski on 05/08/2023.
//

import SwiftCompilerPlugin
import SwiftSyntax
import SwiftSyntaxBuilder
import SwiftSyntaxMacros

extension BuilderBodyGenerator.Configuration {
    static let throwing = Self(flavour: .throwing)
    static let fluent = Self(flavour: .fluent)
    static let storage = Self(flavour: .storage)
}

struct BuilderBodyGenerator {
    fileprivate enum Error: Swift.Error {
        case missingDeclarationName
    }
    
    fileprivate struct TypedVariable {
        let name: String
        let type: String
    }
    
    struct Configuration {
        enum Flavour {
            case plain
            case throwing
            case fluent
            case storage
        }

        let flavour: Flavour

        init(flavour: Flavour = .plain) {
            self.flavour = flavour
        }
    }
    
    private let configuration: Configuration

    init(configuration: Configuration = Configuration()) {
        self.configuration = configuration
    }
    
    func generateBody(from declaration: DeclGroupSyntax) throws -> [DeclSyntax] {
        guard let memberName = declaration.name else {
            throw Error.missingDeclarationName
        }

        return switch configuration.flavour {
        case .plain:
            generateBody(
                memberName: memberName,
                vars: declaration.typedMembers
            )
        case .throwing:
            generateThrowingBody(
                memberName: memberName,
                vars: declaration.typedMembers
            )
        case .fluent:
            generateFluentBody(
                memberName: memberName,
                vars: declaration.typedMembers
            )
        case .storage:
            generateStorageBody(
                memberName: memberName,
                vars: declaration.typedMembers
            )
        }
    }
}

// MARK: Body Generation
extension BuilderBodyGenerator {
    fileprivate func generateBody(
        memberName: String,
        vars: [TypedVariable]
    ) -> [DeclSyntax] {
        ["""
        public class Builder {
        \(raw: vars.publicVariables)
        public init() {
        }

        \(raw: convenienceInitDecl(memberName: memberName))

        public func fill(with item: \(raw: memberName)?) {
            \(raw: vars.fillAssignments)
        }

        public func build() -> \(raw: memberName)? {
            \(raw: vars.buildGuards)
            return \(raw: memberName)(
            \(raw: vars.initAssignments)
            )
        }
        }

        \(raw: makeBuilderDecl())
        """
        ]
    }
    
    fileprivate func generateFluentBody(
        memberName: String,
        vars: [TypedVariable]
    ) -> [DeclSyntax] {//todo make these private variables for the fluent one
        ["""
        public class Builder {
        \(raw: vars.publicVariables)
        public init() {}
        
        \(raw: convenienceInitDecl(memberName: memberName))

        public func fill(with item: \(raw: memberName)?) {
            \(raw: vars.fillAssignments)
        }
        
        \(raw: vars.fluentFunctions)
        
        public func build() -> \(raw: memberName)? {
            \(raw: vars.buildGuards)
            return \(raw: memberName)(
            \(raw: vars.initAssignments)
            )
        }
        }
        
        \(raw: makeBuilderDecl())
        """
        ]
        
    }
    
    fileprivate func generateThrowingBody(
        memberName: String,
        vars: [TypedVariable]
    ) -> [DeclSyntax] {
        ["""
        public class Builder {
        private enum Error: Swift.Error {
            case missingValue(property: String)
        }
        \(raw: vars.publicVariables)
        public init() {
        }

        \(raw: convenienceInitDecl(memberName: memberName))

        public func fill(with item: \(raw: memberName)?) {
            \(raw: vars.fillAssignments)
        }

        public func build() throws -> \(raw: memberName) {
            \(raw: vars.throwingBuildGuards)
            return \(raw: memberName)(
            \(raw: vars.initAssignments)
            )
        }
        }

        \(raw: makeBuilderDecl())
        """
        ]
    }
    
    fileprivate func generateStorageBody(
        memberName: String,
        vars: [TypedVariable]
    ) -> [DeclSyntax] {//todo make these private variables for the fluent one
        ["""
        fileprivate final class Storage {
        \(raw: vars.publicVariables)
        public init() {}
        
        \(raw: convenienceInitDecl(memberName: memberName))

        public func fill(with item: \(raw: memberName)?) {
            \(raw: vars.fillAssignments)
        }
        
        \(raw: vars.fluentFunctions)
        
        public func build() -> \(raw: memberName)? {
            \(raw: vars.buildGuards)
            return \(raw: memberName)(
            \(raw: vars.initAssignments)
            )
        }
        }
        
        public func copy() -> Storage {
            \(raw: vars.buildGuards)
            return Storage(
            \(raw: vars.initAssignments)
            )
        }
        }
        
        private mutating func ensureUniqueness() {
            guard !isKnownUniquelyReferenced(&storage) else { return }
            storage = storage.copy()
        }
        
        \(raw: makeBuilderDecl())
        """
        ]
    }
    
    func generateAccessor(
        providingAccessorsOf declaration: any DeclSyntaxProtocol
    ) -> [AccessorDeclSyntax] {
        guard let property = declaration.as(VariableDeclSyntax.self),
          property.isValidForPerception,
          let identifier = property.identifier?.trimmed
        else {
          return []
        }
        
        let getAccessor: AccessorDeclSyntax =
          """
          get {
          storage.\(identifier)
          }
          """

        let setAccessor: AccessorDeclSyntax =
          """
          set {
          storage.\(identifier) = newValue
          }
          """
        return [getAccessor, setAccessor]
    }

    
    private func convenienceInitDecl(memberName: String) -> String {
        """
        public convenience init(_ item: \(memberName)?) {
            self.init()
            fill(with: item)
        }
        """
    }
    
    private func makeBuilderDecl() -> String {
        """
        public static func makeBuilder() -> Builder {
            Builder()
        }
        """
    }
}

// MARK: Body Fields Construction
extension [BuilderBodyGenerator.TypedVariable] {
    
    var fluentFunctions: String {
        map(\.fluentFunctionDefinition)
        .joined(separator: "\n\n")
    }
    
    var publicVariables: String {
        map(\.publicOptionalVarDefinition)
        .joined(separator: "\n")
    }

    var fillAssignments: String {
        map { $0.assignment(from: "item", isOptional: true) }
        .joined(separator: "\n")
    }
    
    var throwingBuildGuards: String {
        self
            .filter { !$0.isOptional }
            .compactMap(\.throwingGuardCheck)
            .joined()
    }

    var buildGuards: String {
        "guard " + self
            .filter { !$0.isOptional }
            .compactMap(\.guardCheck)
            .joined(separator: ", ")
        + " else { return nil }"
    }

    var initAssignments: String {
        map(\.initAssignment)
        .joined(separator: ",\n")
    }
}

extension BuilderBodyGenerator.TypedVariable {
    func assignment(
        from property: String,
        isOptional: Bool
    ) -> String {
        "\(name) = \(property + (isOptional ? "?" : "")).\(name)"
    }

    var initAssignment: String {
        isUUID
        ? "\(name): \(name) ?? UUID()"
        : "\(name): \(name)"
    }

    var publicOptionalVarDefinition: String {
        "public var \(name): \(optionalType)"
    }
    
    var throwingGuardCheck: String? {
        return isUUID
        ? nil
        : "guard let \(name) else { throw Error.missingValue(property: \"\(name)\") }"
    }

    var guardCheck: String? {
        return isUUID
        ? nil
        : "let \(name)"
    }
    
    var fluentFunctionDefinition: String {
        """
        public func \(name)(_ \(name): \(optionalType)) -> Self {
            self.\(name) = \(name)
            return self
        }
        """
    }

    var isUUID: Bool { name == "uuid" }
    var isOptional: Bool { type.last == "?" }

    private var optionalType: String {
        isOptional ? type : "\(type)?"
    }
}

extension DeclGroupSyntax {
    /// Produces convenience structs from stored properties
    /// with name and type accessible as strings
    fileprivate var typedMembers: [BuilderBodyGenerator.TypedVariable] {
        storedVariables.compactMap { property -> BuilderBodyGenerator.TypedVariable? in
            guard let name = property.name,
                  let type = property.typeString else {
                return nil
            }
            return BuilderBodyGenerator.TypedVariable(
                name: name,
                type: type
            )
        }
    }
}

extension BuilderBodyGenerator.Error: CustomStringConvertible {
    var description: String {
        switch self {
        case .missingDeclarationName:
            return "Unable not find declaration name for type"
        }
    }
}

extension DeclModifierListSyntax {
  func privatePrefixed(_ prefix: String) -> DeclModifierListSyntax {
    let modifier: DeclModifierSyntax = DeclModifierSyntax(name: "private", trailingTrivia: .space)
    return [modifier]
      + filter {
        switch $0.name.tokenKind {
        case .keyword(let keyword):
          switch keyword {
          case .fileprivate, .private, .internal, .package, .public:
            return false
          default:
            return true
          }
        default:
          return true
        }
      }
  }

  init(keyword: Keyword) {
    self.init([DeclModifierSyntax(name: .keyword(keyword))])
  }
}


extension VariableDeclSyntax {
  func privatePrefixed(_ prefix: String, addingAttribute attribute: AttributeSyntax)
    -> VariableDeclSyntax
  {
    let newAttributes = attributes + [.attribute(attribute)]
    return VariableDeclSyntax(
      leadingTrivia: leadingTrivia,
      attributes: newAttributes,
      modifiers: modifiers.privatePrefixed(prefix),
      bindingSpecifier: TokenSyntax(
        bindingSpecifier.tokenKind, leadingTrivia: .space, trailingTrivia: .space,
        presence: .present),
      bindings: bindings.privatePrefixed(prefix),
      trailingTrivia: trailingTrivia
    )
  }

  var isValidForPerception: Bool {
    !isComputed && isInstance && !isImmutable && identifier != nil
  }
}

extension PatternBindingListSyntax {
  func privatePrefixed(_ prefix: String) -> PatternBindingListSyntax {
    var bindings = self.map { $0 }
    for index in 0..<bindings.count {
      let binding = bindings[index]
      if let identifier = binding.pattern.as(IdentifierPatternSyntax.self) {
        bindings[index] = PatternBindingSyntax(
          leadingTrivia: binding.leadingTrivia,
          pattern: IdentifierPatternSyntax(
            leadingTrivia: identifier.leadingTrivia,
            identifier: identifier.identifier.privatePrefixed(prefix),
            trailingTrivia: identifier.trailingTrivia
          ),
          typeAnnotation: binding.typeAnnotation,
          initializer: binding.initializer,
          accessorBlock: binding.accessorBlock,
          trailingComma: binding.trailingComma,
          trailingTrivia: binding.trailingTrivia)

      }
    }

    return PatternBindingListSyntax(bindings)
  }
}

extension TokenSyntax {
  func privatePrefixed(_ prefix: String) -> TokenSyntax {
    switch tokenKind {
    case .identifier(let identifier):
      return TokenSyntax(
        .identifier(prefix + identifier), leadingTrivia: leadingTrivia,
        trailingTrivia: trailingTrivia, presence: presence)
    default:
      return self
    }
  }
}

extension VariableDeclSyntax {
  var identifierPattern: IdentifierPatternSyntax? {
    bindings.first?.pattern.as(IdentifierPatternSyntax.self)
  }

  var isInstance: Bool {
    for modifier in modifiers {
      for token in modifier.tokens(viewMode: .all) {
        if token.tokenKind == .keyword(.static) || token.tokenKind == .keyword(.class) {
          return false
        }
      }
    }
    return true
  }

  var identifier: TokenSyntax? {
    identifierPattern?.identifier
  }

  var type: TypeSyntax? {
    bindings.first?.typeAnnotation?.type
  }

  func accessorsMatching(_ predicate: (TokenKind) -> Bool) -> [AccessorDeclSyntax] {
    let accessors: [AccessorDeclListSyntax.Element] = bindings.compactMap { patternBinding in
      switch patternBinding.accessorBlock?.accessors {
      case .accessors(let accessors):
        return accessors
      default:
        return nil
      }
    }.flatMap { $0 }
    return accessors.compactMap { accessor in
      predicate(accessor.accessorSpecifier.tokenKind) ? accessor : nil
    }
  }

  var willSetAccessors: [AccessorDeclSyntax] {
    accessorsMatching { $0 == .keyword(.willSet) }
  }
  var didSetAccessors: [AccessorDeclSyntax] {
    accessorsMatching { $0 == .keyword(.didSet) }
  }

  var isComputed: Bool {
    if accessorsMatching({ $0 == .keyword(.get) }).count > 0 {
      return true
    } else {
      return bindings.contains { binding in
        if case .getter = binding.accessorBlock?.accessors {
          return true
        } else {
          return false
        }
      }
    }
  }

  var isImmutable: Bool {
    return bindingSpecifier.tokenKind == .keyword(.let)
  }

  func isEquivalent(to other: VariableDeclSyntax) -> Bool {
    if isInstance != other.isInstance {
      return false
    }
    return identifier?.text == other.identifier?.text
  }

  var initializer: InitializerClauseSyntax? {
    bindings.first?.initializer
  }

  func hasMacroApplication(_ name: String) -> Bool {
    for attribute in attributes {
      switch attribute {
      case .attribute(let attr):
        if attr.attributeName.tokens(viewMode: .all).map({ $0.tokenKind }) == [.identifier(name)] {
          return true
        }
      default:
        break
      }
    }
    return false
  }
}
