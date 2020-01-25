:- use_module(library(http/http_open)).
:- use_module(library(http/http_client)).
:- use_module(library(http/json)).
:- use_module(library(url)).
:- use_module(library(http/http_header)).
:- use_module(library(pcre)).

url(Command, URL) :-
    token(Token),
    atomics_to_string(["https://api.telegram.org/bot", Token, "/", Command], URL).

get_updates(Data) :-
    url("getUpdates", URL),
    Headers = [request_header('Accept'='application/json')],
    setup_call_cleanup(
        http_open(URL, In, Headers),
        json_read_dict(In, Data),
        close(In)
    ).

update_offset(Message) :-
    url("getUpdates", URL),
    NewOffset is Message.get(update_id) + 1,
    catch(
        http_post(URL, form_data([offset = NewOffset]), _, []),
        error(_, context(_, status(ErrorCode, Response))),
        (
            format(string(Log), "Couldn't update offset: ~w - ~w", [ErrorCode, Response]),
            log_print(log_level('ERROR'), Log)
        )
    ).

log_print(log_level(LogLevel), Text) :-
    get_time(TimeStamp),
    round(TimeStamp, UnixTimeStamp),
    replace_emoji(Text, EmojiText),
    current_output(Out),
    format(Out, '[~d] [~s] ~s~n', [UnixTimeStamp, LogLevel, EmojiText]).

replace(From,To,In,Out) :-
    maplist(string_codes, [From, To, In],
                          [[FromCode], [ToCode], InCodes]),
    replace_(FromCode, ToCode, InCodes, OutCodes),
    string_codes(Out, OutCodes).

replace_(_,_,[],[]).
replace_(X,Y,[X|T],[Y|NT]) :-
    replace_(X,Y,T,NT),
    !.
replace_(X,Y,[H|T],[H|NT]) :-
    replace_(X,Y,T,NT).


convert_emoji(Text, Emoji) :-
    atom_codes(Text, TextCodes),
    convert_emoji_(TextCodes, EmojiCodes),
    string_codes(Emoji, EmojiCodes).

convert_emoji_([], []) :- !.

convert_emoji_([H,L|T], [Z|T1]) :-
    H >= 55296, H =< 56319,
    L >= 56320, L =< 57343,
    Z is 65536+(H-55296)*1024+(L-56320),
    convert_emoji_(T, T1),
    !.

convert_emoji_([H|T], [H|T1]) :-
    convert_emoji_(T, T1),
    !.

replace_emoji(Text, Text) :-
    not(re_match("\u00a9|\u00ae|[\u2000-\u3300]|\ud83c[\ud000-\udfff]|\ud83d[\ud000-\udfff]|\ud83e[\ud000-\udfff]", Text)),
    !.

replace_emoji(Text, Output) :-
    string_concat(A, B, Text),
    string_concat(X, C, B),
    re_match("^(\u00a9|\u00ae|[\u2000-\u3300]|\ud83c[\ud000-\udfff]|\ud83d[\ud000-\udfff]|\ud83e[\ud000-\udfff])$", X),
    convert_emoji(X, Emoji),
    string_concat(A, Emoji, Head),
    replace_emoji(C, Tail),
    string_concat(Head, Tail, Output),
    !.

array_string(Array, String) :-
    atomics_to_string(Array, '", "', T1),
    string_concat('["', T1, T2),
    string_concat(T2, '"]', String).

save_smokers :-
    open('db/smokers.pl', write, S),
    set_output(S),
    listing(smoker),
    close(S).

get_smokers_on_question(Message, Usernames) :-
    ChatID = Message.get(message).get(chat).get(id),
    chat_id(ChatID),
    MessageText = Message.get(message).get(text),
    upcase_atom(MessageText, MessageUpper),
    string_concat(Office, "?", MessageUpper),
    consult('db/offices'),
    office(_, Office),
    findall(Username, smoker(Username, Office, ChatID), Usernames).

ping(_, []) :- !.

ping(Message, Usernames) :-
    Sender = Message.get(message).get(from).get(username),
    string_concat("@", Sender, SenderUsername),
    delete(Usernames, SenderUsername, UsernamesToSend),
    atomics_to_string(UsernamesToSend, " ", Text),
    send_message(reply(Text),
        Message.get(message).get(message_id),
        Message.get(message).get(chat).get(id)),
    atomics_to_string(["Ping by",
         SenderUsername,
         "to", Text], " ", Log),
    log_print(log_level('INFO'), Log),
    !.

