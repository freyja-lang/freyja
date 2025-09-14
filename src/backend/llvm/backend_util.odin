package llvm_backend

import "../../checker"

// Utility functions (like Odin's llvm_backend_utility.cpp)

// Look up an entity by name in the checker info
lookup_entity_by_name :: proc(info: ^checker.CheckerInfo, name: string) -> ^checker.Entity {
	// Search through all entities for the one with the given name
	// TODO: This is inefficient - we should maintain a name->entity map
	for entity in info.entities {
		if entity.name == name {
			return entity
		}
	}
	return nil
}