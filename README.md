# Preprocessor project

## Kompilace

Určeno pro kompilaci s GHC (využívá různá rozšíření z GHC). Kompilace testována na Windows i Linux verzi GHC. Žádné nestandardní knihovny (mimo defaultní instalaci GHC) nejsou použity.

```
ghc preprocessor.hs
```

## Spuštění

Program očekává dva parametry s cestami k souborům. Výsledek vypíše na standardní výstup.

```
preprocessor data.ppd code.pp
```

Podrobná specifikace k jazyku je v souboru `specfication.txt` (anglicky).

Součástí jazyka jsou výrazy, které se vyhodnotí jako data podobná formátu JSON.
V příkladu výše obsahuje soubor `data.ppd` takový výraz.
Jakýkoliv výraz validní v tomto jazyce je validním v tomto souboru a navíc se ignorují konce řádků.
Výrazy obsahují literály (čísla, boolean, null, řetězce), operátory (prefixové, infixové a circumfixové včetně "list" a "dictionary" literálů) a jména proměnných. Veškerá data jsou typů číslo (dynamicky se mění mezi `Int` a `Double`), řetězec, boolean, null, list a dictionary (resp. array a object jako v JSONu). Operátory (kromě `==`) převádí své operandy na společný typ a žádná operace není chybou. V jazyce není žádný vestavěný konstrukt na explicitní konverzi typů, ale lze toho dosáhnout pomocí různých operátorů, např.: `!!1`, `+"abc"`, `123++""`, `[0,0,0]+3`.

Soubor `code.pp` obsahuje samotný kód preprocesoru.
Tento kód se skládá z příkazů uzavřených v `|...|` resp. `|...\n`, `|...EOF`.
Konce řádků se dají ignorovat explicitně pomocí `\` na konci řádku, nebo pomocí `;`,
ale jen v místech, kde je v kódu `;` validní (příkaz `let`).
Příkazy jsou jednoduché (`|= x|`), blokové (`|for x, y in z| ... |end|`), nebo "multi-blokové" (`|if x| ... |elif y| ... |else| ... |end|`).
Příkazy pak obsahují na určených pozicích další klíčová slova a speciální symboly, jména proměnných a výrazy. V těchto výrazech je možné přistupovat k datům definovaných v souboru `data.ppd` v proměnné `data`. Je to ale opravdu jen předdefinovaná proměnná, ne klíčové slovo, takže je ji možné "zastínit" vnořenými deklaracemi (`let`). Toto se dá využít třeba při dynamickém sestavování html dokumentů, kde se sestaví nějaká statická šablona a ta se pak zparsuje podle dynamických dat.

## Příklady:

Příklad konkrétního vstupu je v souborech `ex1.pp` (kód) a `ex1.ppd` (data).

```
|if showFoo
    foo
|end
```

```
|if show == "foo"
    foo
|elif show == "bar"
    bar
|else
    |if baz
        baz
    |else
        boo
    |end
|end
```

```
lorem|=show|ipsum
```

```
|let a = list[0]; b = list[1]
    |=a
    |=b
|end
```

```
|let a = list[0];
     b = list[1]
    |=a
    |=b
|end
```

```
|for elem in list
    |if elem == "foo"
        FOO
    |else
        |=elem
    |end
|end
```

```
|for value in object
    |=value
|end
```

```
|for key, value in object
    |=key
    |=value
|end
```

```
|let a = 5
    |=a + 1
|end
```