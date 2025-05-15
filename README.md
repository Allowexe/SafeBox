# SafeBox – Projet Erlang TCP avec Mnesia

**SafeBox** est une application client/serveur écrite en Erlang, permettant à un utilisateur de stocker, récupérer et supprimer des secrets (textes) via une connexion TCP. Chaque secret est encodé en Base64 côté client, et **stocké de manière persistante** sur le serveur grâce à **Mnesia**.

---

## Objectifs pédagogiques

- Implémenter un modèle client/serveur avec `gen_tcp`
- Stocker les données de manière persistante avec `Mnesia`
- Gérer plusieurs connexions TCP avec `spawn`
- Séparer les responsabilités client / serveur
- Comprendre l'encodage Base64 côté client

---

## Technologies utilisées

- **Erlang** (OTP 25+ recommandé)
- **TCP/IP** via `gen_tcp`
- **Mnesia** pour le stockage persistant côté serveur
- **Encodage Base64** côté client (`safebox_crypto`)
- **Client CLI** interactif

---

## Structure du projet

```
safebox/
├── src/
│   ├── safebox_server.erl       # Serveur TCP + stockage Mnesia
│   ├── safebox_cli.erl          # Client CLI TCP
│   └── safebox_crypto.erl       # Encodage Base64
├── ebin/                        # Fichiers .beam compilés
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

### Côté Serveur (machine distante)

```bash
erl -pa ebin -sname server -setcookie safebox
```

Puis dans l’interpréteur :

```erlang
c(safebox_server).
safebox_server:start().
```

Le serveur écoute sur `0.0.0.0:5000` et initialise automatiquement **Mnesia** avec une table persistante `secret`.

### Côté Client (depuis une autre machine)

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
> add wifi            # Saisir un secret, l’encoder, l’envoyer
> get wifi            # Récupérer et décoder un secret
> del wifi            # Supprimer un secret
> quit                # Quitter le client
```

---

## Encodage côté client

- Les secrets sont encodés en Base64 avant envoi.
- Le serveur **stocke uniquement la version encodée**, sans jamais voir le contenu en clair.
- Le client décode à la récupération (`get`).

---

## Stockage côté serveur

- Le serveur utilise **Mnesia** pour persister les données.
- La base est automatiquement initialisée si elle n’existe pas.
- Les secrets survivent aux redémarrages du serveur.

---

## Sécurité & Limites

- Le Base64 **n’est pas un chiffrement sécurisé**.
- La communication TCP **n’est pas chiffrée** (pas de TLS).
- Mnesia stocke les secrets encodés, mais non cryptés (prévoir chiffrement fort pour un usage réel).

---

## Évolutions possibles

- Chiffrement AES des secrets côté client
- Authentification utilisateur simple
- Support multi-utilisateur
- Interface Web (Cowboy)
- Sauvegarde/restauration Mnesia

---

## Auteurs

- Clément Veith
- Valentin Scias
- Lucas Ribeiro
- Jean-Baptiste Mattei
- Paul François

---