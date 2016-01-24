musician(tom_petty,vocals).
musician(tom_petty,guitar).
musician(lars_ulrich,drums).
musician(james_hetfield,vocals).
musician(james_hetfield,guitar).
musician(cliff_burton,bass_guitar).
musician(neil_peart,drums).
musician(geddy_lee,bass_guitar).
musician(geddy_lee,keyboards).
musician(geddy_lee,vocals).
musician(eddie_van_halen,guitar).
musician(eddie_van_halen,keyboards).
musician(david_lee_roth,vocals).
musician(mick_jagger,vocals).
musician(keith_richards,backup_vocals).
musician(keith_richards,guitar).

dynamic(Who) :- musician(Who, Role1), musician(Who, Role2), \+(Role1 = Role2).

