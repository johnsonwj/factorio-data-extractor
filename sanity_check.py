import json

with open('data.json') as data_file:
    factorio_data = json.load(data_file)

known_names = dict()

for data_type, data_defs in factorio_data.items():
    if type(data_defs) != dict:
        print(f'expected {data_type} to be a dict')

    for data_name, data_def in data_defs.items():
        qualified_name = f'{data_type}.{data_name}'

        if 'type' not in data_def:
            print(f'expected {qualified_name} to have a type property')
            continue
        if 'name' not in data_def:
            print(f'expected {qualified_name} to have a name property')
            continue
        if data_def['name'] != data_name:
            print(f'expected {qualified_name} to have a matching name property')
            continue

        if data_name not in known_names:
            known_names[data_name] = list()
        known_names[data_name].append(qualified_name)


extra_widget_data_types = ['item', 'fluid', 'gun', 'car', 'module', 'capsule', 'ammo', 'armor', 'cargo-wagon', 'tool', 'artillery-wagon', 'resource', 'locomotive']
widget_names = dict()
for widget_data_type in extra_widget_data_types:
    for widget_name in factorio_data[widget_data_type].keys():
        if widget_name not in widget_names:
            widget_names[widget_name] = list()
        widget_names[widget_name].append(widget_data_type)

item_names = {item['name'] for item in factorio_data['item'].values()}

for recipe_name, recipe in factorio_data['recipe'].items():

    recipe_widget_names = set()

    if 'expensive' in recipe:
        result_src = recipe['normal']
    else:
        result_src = recipe

    if 'result' in result_src:
        recipe_widget_names.add(result_src['result'])
    elif 'results' in result_src:
        for r in result_src['results']:
            if type(r) == list:
                recipe_widget_names.add(r[0])
            elif type(r) == dict:
                recipe_widget_names.add(r['name'])
            elif type(r) == str:
                recipe_widget_names.add(r)
            else:
                print(f'cannot parse recipe results for {recipe_name}')
                continue
    else:
        print(f'cannot find recipe result for {recipe_name}')
        continue

    for ing in result_src['ingredients']:
        if type(ing) == str:
            recipe_widget_names.add(ing)
        elif type(ing) == list:
            recipe_widget_names.add(ing[0])
        elif type(ing) == dict:
            recipe_widget_names.add(ing['name'])
        else:
            print(f'cannot parse ingredient in {recipe_name}')

    for recipe_widget_name in recipe_widget_names:
        if recipe_widget_name not in widget_names:
            possible_matches = [qn for qn in known_names.get(recipe_widget_name, list()) if qn.split('.')[0] not in ['technology', 'recipe']]
            if not possible_matches:
                print(f'  unknown name: {recipe_widget_name}')
            else:
                print(f'  possible matches for {recipe_widget_name}: {possible_matches}')
        elif len(widget_names[recipe_widget_name]) > 1:
            print(f'  multiple possible types for {recipe_widget_name}: {widget_names[recipe_widget_name]}')


