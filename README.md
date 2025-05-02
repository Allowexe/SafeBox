
# 🛡️ SafeBox – Projet Erlang Distribué (ISEN3)

SafeBox est une application Erlang distribuée conçue pour stocker, récupérer et supprimer des secrets textuels de manière sécurisée, tolérante aux pannes et sans persistance. Ce projet met en œuvre les fonctionnalités distribuées d’Erlang dans un cadre pédagogique.

---

## 📁 Structure du projet

```
safebox/
├── ebin/                 # Fichiers compilés (.beam)
├── src/                  # Fichiers source (.erl)
├── test/                 # Scénarios ou tests automatisés (optionnel)
├── config/               # Configurations (liste des nœuds, etc.)
├── launch_nodes.sh       # Script pour lancer les nœuds avec Kitty
├── Makefile              # Compilation et lancement rapides
├── safebox_console.erl   # Lancement CLI intégré
└── README.md             # Ce fichier
```

---

## ⚙️ Pré-requis

- Erlang installé (`erl`)
- Terminal compatible (Kitty recommandé pour `launch_nodes.sh`)
- Système UNIX/Linux (testé sur Zsh + KDE)

---

## 🚀 Compilation et lancement

### 🔧 1. Compiler tous les modules
```bash
make
```

### ▶️ 2. Lancer un shell interactif avec tous les modules
```bash
make run
```

### 💡 3. Ou lancer directement SafeBox en CLI
```bash
make console
```

---

## 🧪 Utilisation interactive (SafeBox CLI)

```erlang
> nodes node1@localhost node2@localhost node3@localhost
> add wifi
Saisir le secret : azerty123
> get wifi
Le secret est : azerty123
> del wifi
> quit
```

---

## 🌐 Lancer 3 nœuds connectés entre eux

### Option 1 : Automatique avec Kitty
```bash
./launch_nodes.sh
```

### Option 2 : Manuelle dans 3 terminaux
```bash
# Terminal 1
erl -sname node1 -setcookie safebox -eval "safebox_node:start()."

# Terminal 2
erl -sname node2 -setcookie safebox -eval "safebox_node:start()."

# Terminal 3
erl -sname node3 -setcookie safebox -eval "safebox_node:start()."
```

#### Se connecter depuis un des nœuds :
```erlang
net_adm:ping('node2@localhost').
net_adm:ping('node3@localhost').
```

Puis :
```erlang
safebox_net:set_nodes(['node1@localhost', 'node2@localhost', 'node3@localhost']).
```

---

## 💾 Fonctionnement technique

- Stockage local par ETS (`safebox_node`)
- Répartition des secrets sur 3 nœuds avec quorum 2/3 (`safebox_net`)
- Chiffrement simple Base64 (`safebox_crypto`)
- Interface CLI interactive (`safebox_cli`)
- Démarrage combiné (`safebox_console`)

---

## 👥 Auteurs

Projet ISEN3 – Algorithmique avancée et systèmes experts (mai 2025)  
Groupe : Valentin, Clément, Lucas, JB, Paul

---

## ⚠️ Note importante sur `make console-all`

La commande `make console-all` peut lancer l'application SafeBox **mais revenir immédiatement au shell Erlang (`1>`)** au lieu de rester dans l'interface `SafeBox CLI`.

### ✅ Solution recommandée

Utilisez la commande suivante pour exécuter correctement SafeBox :

```bash
make run
```

Puis dans le shell Erlang :

```erlang
c(start_safebox),
start_safebox:start().
```

Cela garantit que vous restez bien dans l'interface `SafeBox CLI` jusqu’à ce que vous tapiez `quit`.

---

---

## 🖥️ Lancement manuel des 3 nœuds Erlang (sans script)

Ouvre **3 terminaux séparés** et dans chacun :

### ✅ Terminal 1 – node1
```bash
cd /chemin/vers/ton/projet
make
cd src
erl -pa ../ebin -sname node1@localhost -setcookie safebox
1> safebox_node:start().
```

### ✅ Terminal 2 – node2
```bash
cd /chemin/vers/ton/projet
cd src
erl -pa ../ebin -sname node2@localhost -setcookie safebox
1> safebox_node:start().
```

### ✅ Terminal 3 – node3
```bash
cd /chemin/vers/ton/projet
cd src
erl -pa ../ebin -sname node3@localhost -setcookie safebox
1> safebox_node:start().
```

> 💡 Tu peux vérifier la connexion entre nœuds depuis `node1` :
```erlang
net_adm:ping('node2@localhost').
net_adm:ping('node3@localhost').
```

Une fois les 3 nœuds démarrés, tu peux exécuter :
```erlang
c(start_safebox),
start_safebox:start().
```
ou utiliser `make run` et appeler manuellement `start_safebox:start().` dans un shell centralisé.

---

---

## 🚀 Lancement des nœuds directement depuis la racine du projet

Si vous ne souhaitez pas entrer dans `src/`, vous pouvez lancer chaque nœud directement depuis la racine **après compilation**.

### ✅ Étapes

1. Compiler le projet :
```bash
make
```

2. Ouvrir 3 terminaux séparés et dans chacun :

### ▶️ Terminal 1 – node1
```bash
erl -pa ebin -sname node1 -setcookie safebox -eval "safebox_node:start(), timer:sleep(infinity)."
```

### ▶️ Terminal 2 – node2
```bash
erl -pa ebin -sname node2 -setcookie safebox -eval "safebox_node:start(), timer:sleep(infinity)."
```

### ▶️ Terminal 3 – node3
```bash
erl -pa ebin -sname node3 -setcookie safebox -eval "safebox_node:start(), timer:sleep(infinity)."
```

Cela permet de lancer tous les nœuds SafeBox **depuis la racine** du projet sans entrer dans `src/`.

---
