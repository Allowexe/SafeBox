# SafeBox – Projet Erlang TCP avec Mnesia (multi-utilisateur)

**SafeBox** est une application client/serveur écrite en Erlang, permettant à un utilisateur de stocker, récupérer et supprimer des secrets (textes) via une connexion TCP. Chaque secret est encodé en Base64 côté client, et **stocké de manière persistante** sur le serveur grâce à **Mnesia**, avec **gestion multi-utilisateur**.

---

## Objectifs pédagogiques

- Implémenter un modèle client/serveur avec `gen_tcp`
- Gérer des utilisateurs avec login/mot de passe
- Stocker les données de manière persistante avec `Mnesia`
- Appliquer une architecture modulaire et sécurisée
- Comprendre l'encodage Base64 côté client

---

## Technologies utilisées

- **Erlang** (OTP 25+ recommandé)
- **TCP/IP** via `gen_tcp`
- **Mnesia** pour le stockage persistant
- **Encodage Base64** pédagogique côté client
- **Client CLI** interactif

---

## Structure du projet

```
safebox/
├── src/
│   ├── safebox_server.erl       # Serveur TCP + Mnesia
│   ├── safebox_cli.erl          # Client CLI TCP (multi-utilisateur)
│   └── safebox_crypto.erl       # Encodage Base64
├── ebin/                        # Fichiers compilés
├── Makefile                     # Compilation
└── README.md                    # Ce fichier
```

---

## Compilation

```bash
make
```

---

## Lancement

### Serveur

```bash
erl -pa ebin -sname server -setcookie safebox
```

Puis :

```erlang
c(safebox_server).
safebox_server:start().
```

### Client

```bash
erl -pa ebin -sname client -setcookie safebox
```

Puis :

```erlang
c(safebox_cli).
safebox_cli:start("IP_DU_SERVEUR").
```

---

## Commandes disponibles (client CLI)

```
> register <utilisateur>     # Créer un nouvel utilisateur
> login <utilisateur>        # Se connecter avec mot de passe
> add <clé>                  # Ajouter un secret (login requis)
> get <clé>                  # Lire un secret
> del <clé>                  # Supprimer un secret
> quit                       # Quitter le client
```

ℹ️ Le **login actif est automatiquement transmis au serveur** dans toutes les commandes `add`, `get`, `del`.

---

## Sécurité & encodage

- Le secret est **encodé en Base64** côté client.
- Le serveur ne voit pas les données en clair.
- Pour plus de sécurité, un chiffrement fort (AES) est recommandé.

---

## Stockage côté serveur

- Données **persistées** dans Mnesia avec tables `user` et `secret`.
- Chaque secret est lié à un utilisateur.
- Les données survivent aux redémarrages du serveur.

---

## Limites et pistes d'amélioration

- Ajouter un vrai chiffrement (AES)
- Liste des clés (`list`)
- Interface Web avec Cowboy
- Export/sauvegarde Mnesia
- Session persistante avec gestion de tokens

---

## Auteurs

- Clément Veith
- Valentin Scias
- Lucas Ribeiro
- Jean-Baptiste Mattei
- Paul François

---