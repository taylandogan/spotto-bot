# spotto-bot

A Haskell client that retrieves your playlists and exports them as .csv files.
Note that this piece of code can only access to your public playlists.
The resulting **PLAYLIST_NAME**.csv file will have the following structure:


| Artist                  | Name             | ID                     | Popularity |
| ------------------------|:----------------:|:----------------------:|-----------:|
| CCFX                    | The One to Wait  | 3MPFzBWdqwaAeCSQpA7Ift | 48         |
| Jefferson Airplane      | Today            | 0yLfXULLuRtVj2L4DIrwO7 | 58         |
| ... | ...      | ... | ... |

## How to run the script

- Go to Spotify Developers Dashboard
- Create an app
- Get your CLIENT_ID and CLIENT_SECRET.

##### Then run the following commands:

- export **SPOTIFY_APP_ID**=[YOUR_APP_ID]
- export **SPOTIFY_APP_SECRET**=[YOUR_APP_SECRET]
- **stack build**
- **stack exec spotto-bot-exe**

## You need to:

- Make your playlists public
- Change your username in **Const.hs**
- Run the script!
