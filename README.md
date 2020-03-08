# spotto-bot

A Haskell client that retrieves your playlists and exports them as .csv files.

The resulting **PLAYLIST_NAME**.csv file will have the following structure:


| Artist                  | Name             | ID                     | Popularity |
| ------------------------|:----------------:|:----------------------:|-----------:|
| CCFX                    | The One to Wait  | 3MPFzBWdqwaAeCSQpA7Ift | 48         |
| Jefferson Airplane      | Today            | 0yLfXULLuRtVj2L4DIrwO7 | 58         |
| ... | ...      | ... | ... |


## You need to:

- Change your username in **Const.hs**
