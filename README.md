
#  SafeBox – Projet Erlang Distribué (ISEN3)

**SafeBox** est une application distribuée écrite en Erlang, permettant à un utilisateur de stocker, récupérer et supprimer des secrets (textes) de manière sécurisée, redondante, et sans persistance. Elle est conçue pour démontrer les capacités d’Erlang en matière de programmation fonctionnelle, concurrente et distribuée.

---

##  Objectifs pédagogiques

- Mettre en œuvre un système client/serveur distribué avec plusieurs nœuds Erlang
- Appliquer la tolérance aux pannes via un mécanisme de quorum
- Utiliser ETS pour le stockage en mémoire
- Développer une interface CLI interactive
- Respecter la logique Agile : sprints, modularité, testabilité

---

##  Technologies utilisées

- Langage : **Erlang**
- Stockage local : **ETS (in-memory)**
- Communication : **RPC inter-nœuds** avec `rpc:call/4`
- Chiffrement : **Base64** pédagogique
- Interface : **Ligne de commande** (`io:get_line/1`)
- Nœuds : **3 nœuds minimum** (nommés `node1`, `node2`, `node3`)

---

##  Structure du projet

```
safebox/
├── src/                     # Modules source (.erl)
├── ebin/                    # Fichiers compilés (.beam)
├── Makefile                 # Compilation & exécution
├── README.md                # Présentation du projet
├── start_safebox.erl        # Démarrage centralisé du client CLI
```

---

##  Lancer le projet (manuel)

### 1. Compiler
```bash
make
```

### 2. Ouvrir 3 terminaux pour les nœuds

#### Terminal 1 :
```bash
erl -pa ebin -sname node1 -setcookie safebox -eval "safebox_node:start(), timer:sleep(infinity)."
```

#### Terminal 2 :
```bash
erl -pa ebin -sname node2 -setcookie safebox -eval "safebox_node:start(), timer:sleep(infinity)."
```

#### Terminal 3 :
```bash
erl -pa ebin -sname node3 -setcookie safebox -eval "safebox_node:start(), timer:sleep(infinity)."
```

### 3. Lancer le client dans un 4ᵉ terminal
```bash
erl -pa ebin -sname client -setcookie safebox
```

```erlang
net_adm:ping('node1@localhost').
net_adm:ping('node2@localhost').
net_adm:ping('node3@localhost').

c(start_safebox),
start_safebox:start().
```

---

##  Commandes disponibles (CLI SafeBox)

```
> nodes [liste]       # ex: nodes node1@localhost node2@localhost node3@localhost
> add wifi            # ajoute un secret chiffré
> get wifi            # récupère un secret
> del wifi            # supprime un secret
> quit                # quitter le programme
```

---

##  Fonctionnement technique

- `safebox_node.erl` : gère le stockage local via ETS
- `safebox_net.erl` : distribue les appels, applique le quorum 2/3
- `safebox_crypto.erl` : encode/décode les secrets en Base64
- `safebox_cli.erl` : interprète les commandes utilisateur
- `start_safebox.erl` : init du client + CLI

---

##  Comportement attendu

| Scénario                                 | Résultat attendu                     |
|------------------------------------------|--------------------------------------|
| Ajout d’un secret                        | Secret chiffré, distribué sur 3 nœuds|
| Lecture avec 3 ou 2 nœuds actifs         |   Succès : grâce au quorum           |
| Lecture avec 1 seul nœud actif           |   Échec : quorum insuffisant         |
| Suppression d’un secret                  | Suppression répartie                 |
| Redémarrage complet                      | Les données sont perdues (volatilité)|

---

##  Membres du groupe

- Valentin
- Clément
- Lucas
- JB
- Paul

---