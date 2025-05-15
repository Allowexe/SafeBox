# SafeBox – Projet Erlang TCP (Client/Serveur)

**SafeBox** est une application client/serveur écrite en Erlang, permettant à un utilisateur de stocker, récupérer et supprimer des secrets (textes) de manière sécurisée via une connexion TCP. Chaque secret est chiffré côté client et stocké en mémoire côté serveur.

---

## Objectifs pédagogiques

- Implémenter un vrai modèle client/serveur avec `gen_tcp`
- Gérer des connexions TCP simultanées avec `spawn`
- Stocker les données en mémoire avec `ETS`
- Appliquer une logique modulaire
- Comprendre les échanges binaires sur socket

---

## Technologies utilisées

- **Erlang** (OTP 25+ recommandé)
- **TCP/IP** via `gen_tcp`
- **Chiffrement** Base64 pédagogique (via `safebox_crypto`)
- **ETS** pour le stockage temporaire
- **Client CLI** interactif

---

## Structure du projet

```
safebox/
├── src/
│   ├── safebox_server.erl       # Serveur TCP (écoute sur 0.0.0.0:5000)
│   ├── safebox_cli.erl          # Client CLI TCP
│   └── safebox_crypto.erl       # Module de chiffrement (Base64)
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

Dans l’interpréteur :

```erlang
c(safebox_server).
safebox_server:start().
```

Le serveur écoute sur le port `5000` et toutes les IPs (`0.0.0.0`)

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

## Commandes disponibles (en ligne de commande)

```
> add wifi            # Saisit un secret, le chiffre, l’envoie au serveur
> get wifi            # Récupère le secret et le déchiffre
> del wifi            # Supprime le secret côté serveur
> quit                # Quitte le client
```

---

## Chiffrement

- Le chiffrement est actuellement fait en Base64 côté client.
- Le serveur ne connaît pas le contenu original.
- L’algorithme est modulaire (via `safebox_crypto.erl`) → peut être remplacé par AES.

---

## Sécurité & Limites

- Le chiffrement Base64 est **pédagogique uniquement** (non sécurisé).
- Le serveur **ne persiste pas les données** (ETS = mémoire).
- La communication n’est **pas chiffrée** sur le réseau (pas de TLS).

---

## Idées d’évolution

- Remplacer Base64 par AES (avec `crypto:block_encrypt`)
- Ajouter une authentification simple (login/mot de passe)
- Support multi-utilisateur
- Interface Web via Cowboy
- Persistance disque (ex: Mnesia ou fichier)

---

## Auteurs

- Clément Veith
- Valentin Scias
- Lucas Ribeiro
- Jean-Baptiste Mattei
- Paul François

---
