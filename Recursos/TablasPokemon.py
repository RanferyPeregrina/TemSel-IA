import requests
import pandas as pd

def obtener_datos_pokemon(tipo):
    url = f"https://pokeapi.co/api/v2/type/{tipo.lower()}"
    respuesta = requests.get(url)

    if respuesta.status_code == 200:
        datos_tipo = respuesta.json()
        return datos_tipo
    else:
        print(f"Hubo un error al obtener información para el tipo {tipo}")
        return None

def obtener_pokemon_por_tipo(tipo):
    datos_tipo = obtener_datos_pokemon(tipo)

    if datos_tipo:
        pokemones = datos_tipo['pokemon']
        return [pokemon['pokemon']['name'] for pokemon in pokemones]
    else:
        return []

def obtener_estadisticas_pokemon(nombre_pokemon):
    url = f"https://pokeapi.co/api/v2/pokemon/{nombre_pokemon.lower()}"
    respuesta = requests.get(url)

    if respuesta.status_code == 200:
        datos_pokemon = respuesta.json()
        return datos_pokemon
    else:
        print(f"No se pudo obtener información para el Pokémon {nombre_pokemon}")
        return None

def main():
    TipoPokemon = input("Ingresa el tipo de pokémon (en inglés) que quieras buscar: ")
    BusquedaTipo = obtener_pokemon_por_tipo(TipoPokemon)

    # Crear un DataFrame de pandas para almacenar los datos
    df = pd.DataFrame(columns=['Nombre', 'HP', 'Ataque', 'Defensa', 'Velocidad', 'Ataque Especial', 'Defensa Especial'])

    for nombre_pokemon in BusquedaTipo:
        estadisticas_pokemon = obtener_estadisticas_pokemon(nombre_pokemon)
        if estadisticas_pokemon:
            # Extraer las estadísticas relevantes
            hp = next(stat['base_stat'] for stat in estadisticas_pokemon['stats'] if stat['stat']['name'] == 'hp')
            ataque = next(stat['base_stat'] for stat in estadisticas_pokemon['stats'] if stat['stat']['name'] == 'attack')
            defensa = next(stat['base_stat'] for stat in estadisticas_pokemon['stats'] if stat['stat']['name'] == 'defense')
            velocidad = next(stat['base_stat'] for stat in estadisticas_pokemon['stats'] if stat['stat']['name'] == 'speed')
            ataque_especial = next(stat['base_stat'] for stat in estadisticas_pokemon['stats'] if stat['stat']['name'] == 'special-attack')
            defensa_especial = next(stat['base_stat'] for stat in estadisticas_pokemon['stats'] if stat['stat']['name'] == 'special-defense')

            # Agregar una nueva fila al DataFrame
            df.loc[len(df.index)] = [nombre_pokemon, hp, ataque, defensa, velocidad, ataque_especial, defensa_especial]


    # Guardar el DataFrame en un archivo Excel
    print("\n =========================================")
    print("Todo salió bien.")
    print("Ahora hay que convertir tu archivo en un Excel.")
    NombreExcel = input("Ingresa el nombre de tu excel: ")
    NombreExcelConExtension = NombreExcel + ".xlsx"
    print("Convirtiendo tu archivo a un Excel...")
    print("No cierres el programa...")
    df.to_excel(NombreExcelConExtension, index=False)

if __name__ == "__main__":
    main()
