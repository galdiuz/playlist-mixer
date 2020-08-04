# Playlist Mixer

Playlist Mixer is a client-side web application written in Elm that lets you shuffle multiple
YouTube playlists together into a single big list and then play them in an embedded YouTube player.

[Try it here!](https://galdiuz.github.io/playlist-mixer)


## Purpose of the app

I like to use YouTube to listen to music, but it's difficult to manage large playlists in YouTube.
Thus I like to distribute what I listen to into different lists, but I would still like to be able
to shuffle everything together. It is also common for videos with music to have intros and outros
which I would like to skip, but ever since YouTube discontinued the start- and end-time functionality
of playlists that has not been available natively.


## Features

- Mix multiple playlists into a single one.
- Mixed playlist and current position is saved to local storage to allow resuming from where you left off.
- Configure start and stop time of videos to skip intros and/or outros of videos (by saving a note on the video in the playlist).
