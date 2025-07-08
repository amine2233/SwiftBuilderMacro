import BuilderMacro
import Foundation

// MARK: Example

@Builder
struct Breathing {
    let uuid: UUID
    let duration: Double
    let thoughts: String?
}

@ThrowingBuilder
struct Player {
    let uuid: UUID
    let coins: Int
    let hp: Int
}

@FluentBuilder
struct Quote {
    let uuid: UUID
    let text: String
    let author: String
}

let quote = Quote.makeBuilder()
    .text("What a time to be alive")
    .author("A dude")
    .build()

print(String(describing: quote))

let throwingBuilder = Player.makeBuilder()

do {
    throwingBuilder.coins = 100
    throwingBuilder.hp = 10
    let player = try throwingBuilder.build()
    print(player)
} catch {
    print(error)
}

let builder = Breathing.makeBuilder()
builder.duration = 60

print(String(describing: builder.build()))


struct Home {
    let uuid: UUID
    let name: String
    
    
    
    
    fileprivate var storage = Storage()
    
    fileprivate final class Storage {
        var uuid: UUID?
        var name: String?
        init() {}
        
        convenience init(_ item: Home?) {
            self.init()
            fill(with: item)
        }
        
        init(
            uuid: UUID?,
            name: String?
        ) {
            self.uuid = uuid
            self.name = name
        }
        
        func fill(with item: Home?) {
            uuid = item?.uuid
            name = item?.name
        }
        
        public func withUuid(_ uuid: UUID?) -> Self {
            self.uuid = uuid
            return self
        }
        
        public func withName(_ name: String?) -> Self {
            self.name = name
            return self
        }
        
        func build() -> Home? {
            guard let name else { return nil }
            return Home(
                uuid: uuid ?? UUID(),
                name: name
            )
        }
        
        func copy() -> Storage {
            return Storage(
                uuid: self.uuid,
                name: self.name
            )
        }
    }
    
    private mutating func ensureUniqueness() {
        guard !isKnownUniquelyReferenced(&storage) else { return }
        storage = storage.copy()
    }
    
    public class Builder {
        public var uuid: UUID?
        public var name: String?
        public init() {}
        
        public convenience init(_ item: Home?) {
            self.init()
            fill(with: item)
        }
        
        public func fill(with item: Home?) {
            uuid = item?.uuid
            name = item?.name
        }
        
        public func withUuid(_ uuid: UUID?) -> Self {
            self.uuid = uuid
            return self
        }
        
        public func withName(_ name: String?) -> Self {
            self.name = name
            return self
        }
        
        public func build() -> Home? {
            guard let name else { return nil }
            return Home(
                uuid: uuid ?? UUID(),
                name: name
            )
        }
    }
    
    public static func makeBuilder() -> Builder {
        Builder()
    }
}

let storage = Home.makeBuilder()
    .withUuid(UUID())
    .withName("My House")

print(String(describing: storage.build()))

storage.name = "My New House"

print(storage)
