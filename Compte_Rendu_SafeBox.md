
#  Compte rendu – SafeBox (Projet Erlang ISEN3)

##  Fonctionnalités développées

-  Stockage sécurisé de secrets (textes) en mémoire avec chiffrement Base64
-  Répartition des données sur 3 nœuds Erlang connectés
-  Tolérance aux pannes avec quorum (lecture possible si 2/3 nœuds sont en ligne)
-  Interface utilisateur en ligne de commande (CLI) interactive
-  Suppression de secrets
-  Commande `nodes` pour consulter et définir dynamiquement les nœuds
-  Aucun stockage persistant (tout est en RAM)

---

##  WBS – Work Breakdown Structure

1. **Initialisation du projet**
   - Création du dépôt
   - Écriture de la charte
   - Répartition des rôles

2. **Développement des modules**
   - `safebox_node.erl` – stockage ETS
   - `safebox_crypto.erl` – chiffrement Base64
   - `safebox_net.erl` – gestion inter-nœuds / quorum
   - `safebox_cli.erl` – interface CLI
   - `start_safebox.erl` – démarrage centralisé

3. **Infrastructure distribuée**
   - Lancement de 3 nœuds manuels
   - Test des connexions avec `net_adm:ping/1`

4. **Interface et tests**
   - Implémentation de la CLI avec commandes : `add`, `get`, `del`, `nodes`, `quit`
   - Scénarios de test : ajout, récupération, quorum, suppression, redémarrage

5. **Documentation et livrables**
   - README complet
   - Charte projet
   - Plan de test
   - Compte rendu + RACI

---

##  Matrice RACI

| Tâche                           | Valentin | Clément | Lucas | JB   | Paul |
|--------------------------------|----------|---------|-------|------|------|
| Écriture de la charte          | R        | A       | C     | C    | I    |
| Développement du stockage ETS  | A        | R       | C     | I    | I    |
| Module de chiffrement          | C        | A       | R     | I    | I    |
| Communication inter-nœuds      | C        | C       | A/R   | I    | I    |
| CLI utilisateur                | C        | I       | I     | A/R  | C    |
| Scénarios de test              | R        | C       | C     | C    | A    |
| Documentation finale           | A        | R       | C     | C    | C    |

**R** = Responsable, **A** = Autorité (décide), **C** = Consulté, **I** = Informé

---
