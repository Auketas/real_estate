# -*- coding: utf-8 -*-
"""
Builds dashboard/static/neighbourhood_lookup.json
Maps scraper neighbourhood names -> GeoJSON NAME_3 feature names (Porto/Lisboa)
Run from repo root: python code/scripts/build_neighbourhood_lookup.py
"""
import json, os

OUT = os.path.join(os.path.dirname(__file__), "..", "..", "dashboard", "static",
                   "neighbourhood_lookup.json")

# ── Porto region ──────────────────────────────────────────────────────────────
# GADM parishes are coarser than scraper names (7 Porto parishes, 15 Matosinhos).
# Most Porto city names are sub-parish; we map what we can confidently assign.

PORTO = {
    # Direct / near-direct parish matches
    "Cedofeita":           "Cedofeita",
    "Ramalde":             "Ramalde",
    "Paranhos":            "Paranhos",
    "Miragaia":            "Miragaia",
    "Bonfim":              "Bonfim",
    "Vitória":             "Vitória",
    "Sé":                  "Sé",
    "Massarelos":          "Massarelos",
    "Nevogilde":           "Nevogilde",

    # Matosinhos parishes
    "Matosinhos":          "Matosinhos",
    "Matosinhos Sul":      "Matosinhos",
    "Leça da Palmeira":    "LeçaDaPalmeira",
    "Leça do Balio":       "LeçaDoBalio",
    "Custóias":            "Custóias",
    "Senhora da Hora":     "SenhoraDaHora",
    "São Mamede de Infesta":"SãoMamedeDeInfesta",
    "Santa Cruz do Bispo": "SantaCruzDoBispo",
    "Perafita":            "Perafita",

    # Sub-parish Porto names mapped to their parish
    "Baixa do Porto":      "Sé",
    "Barredo":             "Miragaia",
    "São João Novo":       "Miragaia",
    "Ilhéu":               "SãoNicolau",
    "Clérigos":            "SantoIldefonso",
    "Marquês":             "Cedofeita",
    "Leal":                "Cedofeita",
    "Picaria":             "Cedofeita",
    "Caldeireiros":        "Cedofeita",
    "Virtudes":            "Cedofeita",
    "Campo Alegre":        "Massarelos",
    "Arrábida":            "Massarelos",
    "Cantareira":          "Massarelos",
    "Boavista":            "LordeloDoOuro",
    "Aleixo":              "LordeloDoOuro",
    "Sobreiras":           "LordeloDoOuro",
    "Serralves":           "Nevogilde",
    "Castelo do Queijo":   "Nevogilde",
    "Figueiroa":           "Nevogilde",
    "Monte Cativo":        "Paranhos",
    "Bom Retiro":          "Paranhos",
    "Carvalhosa":          "Paranhos",
    "Monte da Luz":        "Paranhos",
    "Agra do Amial":       "Paranhos",
    "Monte Aventino":      "Paranhos",
    "Bom Pastor":          "Paranhos",
    "Bouça":               "Paranhos",
    "Monte dos Burgos":    "Paranhos",
    "Congostas":           "Paranhos",
    "Foco":                "Paranhos",
    "Monte dos Congregados":"Paranhos",
    "Ramada Alta":         "Ramalde",
    "Monte de Ramalde":    "Ramalde",
    "Telheira":            "Ramalde",
    "Vilarinha":           "Ramalde",
    "Viso de Baixo":       "Ramalde",
    "Viso de Cima":        "Ramalde",
    "Bessa":               "Aldoar",
    "Areosa":              "Campanhã",
    "Contumil":            "Campanhã",
    "Lagarteiro":          "Campanhã",
    "Lameira":             "Campanhã",
    "Corujeira":           "Campanhã",
    "Antas":               "Campanhã",
    "Estação":             "Campanhã",
    "Bairro do Lagarteiro":"Campanhã",
    "Bairro de Costa Cabral":"Paranhos",
    "Monte do Bonfim":     "Bonfim",
    "Barrocas":            "Bonfim",
    "Avis":                "Bonfim",
    "Fontainhas":          "Bonfim",
    "São Crispim":         "Bonfim",

    # Vila Nova de Gaia parishes
    "Santa Marinha":               "VilaNovaDeGaia(SantaMarinha",
    "Alumiara":                    "VilaNovaDeGaia(SantaMarinha",
    "Quinta Marques Gomes":        "VilaNovaDeGaia(SantaMarinha",
    "Quebrantães":                 "VilaNovaDeGaia(SantaMarinha",
    "Lavadores":                   "VilaNovaDeGaia(SantaMarinha",
    "Mafamude":                    "Mafamude",
    "Laborim":                     "Mafamude",
    "Oliveira do Douro":           "OliveiraDoDouro",
    "São Pedro da Afurada":        "SãoPedroDaAfurada",
    "Canidelo":                    "Canidelo",
    "Vilar de Andorinho":          "VilarDeAndorinho",
    "Vilar do Paraíso":            "VilarDoParaíso",
    "Aguda":                       "Arcozelo",
    "Granja":                      "Arcozelo",
}

# ── Lisboa region ─────────────────────────────────────────────────────────────
# Lisboa 2012 reform merged 53 old parishes into 24.
# Many DB names are old parish names or informal names mapped to current parishes.

LISBOA = {
    # Direct matches (current parishes)
    "Anjos":                    "Anjos",
    "Benfica":                  "Benfica",
    "Campo Grande":             "CampoGrande",
    "Campolide":                "Campolide",
    "Carnide":                  "Carnide",
    "Coração de Jesus":         "CoraçãoDeJesus",
    "Encarnação":               "Encarnação",
    "Graça":                    "Graça",
    "Lapa":                     "Lapa",
    "Lumiar":                   "Lumiar",
    "Mercês":                   "Mercês",
    "Mártires":                 "Mártires",
    "Pena":                     "Pena",
    "Prazeres":                 "Prazeres",
    "Sacramento":               "Sacramento",
    "Santa Catarina":           "SantaCatarina",
    "Santa Engrácia":           "SantaEngrácia",
    "Santa Isabel":             "SantaIsabel",
    "Santa Justa":              "SantaJusta",
    "Santiago":                 "Santiago",
    "Santo Condestável":        "SantoCondestável",
    "Santos-o-Velho":           "Santos-O-Velho",
    "Socorro":                  "Socorro",
    "São Cristóvão e São Lourenço": "SãoCristóvãoESãoLourenço",
    "São Francisco Xavier":     "SãoFranciscoXavier",
    "São João":                 "SãoJoão",
    "São João de Brito":        "SãoJoãoDeBrito",
    "São José":                 "SãoJosé",
    "São Mamede":               "SãoMamede",
    "São Nicolau":              "SãoNicolau",
    "São Paulo":                "SãoPaulo",
    "São Sebastião da Pedreira":"SãoSebastiãoDaPedreira",
    "São Vicente de Fora":      "SãoVicenteDeFora",
    "Sé":                       "Sé",
    "Beato":                    "Beato",
    "Marvila":                  "Marvila",
    "Alvalade":                 "Alvalade",
    "Ajuda":                    "Ajuda",

    # Bairro names mapped to their current parish
    "Bairro Alto":              "Mercês",
    "Bairro da Encarnação":     "Encarnação",
    "Bairro de Campolide":      "Campolide",
    "Bairro de Santa Cruz de Benfica": "Benfica",
    "Bairro de São Miguel":     "SãoMiguel",
    "Bairro Andrade":           "SãoJorgeDeArroios",
    "Bairro Azul":              "NossaSenhoraDeFátima",
    "Bairro Lopes":             "SãoJorgeDeArroios",
    "Bairro Oriente":           "Marvila",
    "Bairro da Calçada dos Mestres": "SantaMariaDeBelém",
    "Bairro das Amendoeiras":   "Marvila",
    "Bairro das Novas Nações":  "SantaMariaDosOlivais",
    "Bairro de Inglaterra":     "SantaMariaDeBelém",
    "Bairro do Rego":           "Campolide",
    "Bairro dos Actores":       "PenhaDeFrança",

    # Informal / tourist names mapped to current parish
    "Alfama":                   "SãoVicenteDeFora",
    "Chiado":                   "Sacramento",
    "Mouraria":                 "Socorro",
    "Madragoa":                 "Santos-O-Velho",
    "Cais do Sodrê":            "Mártires",
    "Areeiro":                  "SãoJorgeDeArroios",
    "Saldanha":                 "NossaSenhoraDeFátima",
    "Campo Pequeno":            "NossaSenhoraDeFátima",
    "Rato":                     "Campolide",
    "Amoreiras":                "Campolide",
    "Casal Ventoso":            "Campolide",
    "Santo Amaro":              "Alcântara",
    "Junqueira":                "SantaMariaDeBelém",
    "Restelo":                  "SantaMariaDeBelém",
    "Pedrouços":                "Ajuda",
    "Ceuta Sul":                "Ajuda",
    "Alta de Lisboa":           "Ameixoeira",
    "Olivais Norte":            "SantaMariaDosOlivais",
    "Olivais Sul":              "SantaMariaDosOlivais",
    "Parque das Nações":        "SantaMariaDosOlivais",
    "Bairro das Novas Nações":  "SantaMariaDosOlivais",
    "Poço do Bispo":            "Beato",
    "Picheleira":               "Marvila",
    "São Bento":                "Mercês",
    "Alto da Faia":             "Marvila",
    "Telheiras Sul":            "Lumiar",
    "Quinta das Laranjeiras":   "Alvalade",
    "Quinta do Cabrinha":       "Alcântara",
    "Quinta do Lambert":        "Alvalade",
    "Santa Maria Maior":        "Castelo",
    "Alto da Lisboa":           "Ameixoeira",

    # Cascais fallback
    "cascais":                  "Cascais",
}

lookup = {**PORTO, **LISBOA}

os.makedirs(os.path.dirname(OUT), exist_ok=True)
with open(OUT, "w", encoding="utf-8") as f:
    json.dump(lookup, f, ensure_ascii=False, indent=2, sort_keys=True)

print(f"Written {len(lookup)} entries to {OUT}")
