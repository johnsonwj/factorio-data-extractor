## sanity checks

- verify that there are no recipe ingredients or results that are not an `item`
- verify that there isn't anything with the same `name` but different `types`, except for `item` / `recipe`

## how to pull out relevant data

types that can contain extra material data:
* item
* fluid
* gun
* car
* module
* capsule
* ammo
* armor
* cargo-wagon
* tool
* artillery-wagon
* resource
* locomotive
* fluid-wagon
* repair-tool
* rail-planner

data.raw is organized by type

1. keep everything with type `recipe` (unless it has a hidden flag)
1. keep everything with type `item` (unless it has a hidden flag)
1. keep everything whose `name` matches the `place_result` of any `item`
    * algo: iterate over types, checking to see if the item `name` is a property
        - if this is a bottleneck, try folding data.raw into a hashmap keyed by `name`
            ...but probably the number of named things is small enough that this should be fine
    * move it to a new `entity_details` property in the matching `item`
    * always discard these irrelevant properties from the entity_details:
        - circuit_wire_connection_points
        - *_box
        - *_pictures
        - *_sprites
        - *_animation
        - *_graphics
        - fast_replaceable_group
        - icon_size
        - fluid_wagon_connector_frame_count
        - circuit_wire_max_distance
        - animations
        - corpse
        - resistances
        - vehicle_impact_sound
        - infinite
        - sprite
        - shape
        - structure
        - structure_animation_movement_cooldown
        - belt_animation_set
        - autoplace

