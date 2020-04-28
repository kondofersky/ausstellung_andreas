# Hintergrund
Häufig hat man selbst oder jemand anderes zu einem Thema schon einmal einen Code geschrieben.
Jetzt möchte man diesen Code für seine eigenen Zwecke verwenden, aber weiß nicht mehr, wie genau dieser File hieß.
Um zu vermeiden, dass man in seinem File-Wust die Nadel im Heuhaufen suchen muss, soll dieses Tool helfen.

# Anleitung
Öffne: StringSearchInFiles.R
Führe Funktion StringSearchInFiles() aus. Parameter:
- folder: Folder you want to search. Default: 'C:/nanoathenareports'
- string: What you are searching for inside the code in Regex. Default: 'iasviewability'
- myignorecase: should your search be case insensivite? Default: TRUE
- datefilter: Add a 'date modified' filter, if you want to. Only result younger than this will be shown. Default: NULL (means no date filter)
- pattern: Pattern of Files names to search through. Default: R, python and SQL -files)
- surroundingarea: # of characters returned before and after the found string, Default: 20

# Ergebniss:
Übersicht aller Codes mit dem Pattern in dem definierten Ordner und Subordner. Und eine Vorschau der Pattern-Umgebung in den einzelnen Files.