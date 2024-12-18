/// A macro that produces Builder
@attached(member, names: arbitrary)
public macro Builder() = #externalMacro(
    module: "BuilderMacroMacros",
    type: "BuilderMacro"
)

/// A macro that produces ThrowingBuilder
@attached(member, names: arbitrary)
public macro ThrowingBuilder() = #externalMacro(
    module: "BuilderMacroMacros",
    type: "ThrowingBuilderMacro"
)

/// A macro that produces FluentBuilder
@attached(member, names: arbitrary)
public macro FluentBuilder() = #externalMacro(
    module: "BuilderMacroMacros",
    type: "FluentBuilderMacro"
)

/// A macro that produces FluentBuilder
@attached(member, names: arbitrary)
public macro StorageBuilder() = #externalMacro(
    module: "BuilderMacroMacros",
    type: "StorageBuilderMacro"
)
