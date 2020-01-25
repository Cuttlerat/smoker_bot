:- use_module(library(http/http_open)).
:- use_module(library(http/http_client)).
:- use_module(library(http/json)).
:- use_module(library(clpfd)).

?- consult('db/config').
?- consult('db/smokers').
?- consult('db/offices').
?- consult(utils).

:- dynamic current_poll/2.


send_message(Text, MessageID, ChatID) :-
    string(Text),
    send_message(reply(Text), MessageID, ChatID),
    !.

send_message(reply(Text), MessageID, ChatID) :-
    url("sendMessage", URL),
    replace_emoji(Text, EmojiText),
    catch(
        http_post(URL,
            form_data([
                text = EmojiText,
                reply_to_message_id = MessageID,
                chat_id = ChatID
            ]), _, []
        ),
        error(_, context(_, status(_, _))),
        send_message(no_reply(Text), _, ChatID)
    ),
    !.

send_message(no_reply(Text), _, ChatID) :-
    url("sendMessage", URL),
    replace_emoji(Text, EmojiText),
    catch(
        http_post(URL,
            form_data([
                text = EmojiText,
                chat_id = ChatID
            ]), _, []
        ),
        error(_, context(_, status(ErrorCode, Response))),
        (
            format(string(Log), "Couldn't send message: ~w - ~w", [ErrorCode, Response]),
            log_print(log_level('ERROR'), Log)
        )
    ).

send_poll :-
    foreach(office(ID, Office), nth0(ID, Offices, Office)),
    findall(X, office(X, _), L),
    length(L, N),
    length(Offices, N),
    array_string(Offices, Options),
    foreach(chat_id(X), send_poll("?", Options, X)),
    !.

send_poll(Question, Options, ChatID) :-
    url("sendPoll", URL),
    catch(
        http_post(URL,
            form_data([
                question = Question,
                options = Options,
                'is_anonymous' = "False",
                chat_id = ChatID
            ]), Response, []
        ),
        error(_, context(_, status(ErrorCode, Response))),
        (
            format(string(Log), "Couldn't send Poll: ~w - ~w", [ErrorCode, Response]),
            log_print(log_level('ERROR'), Log)
        )
    ),
    atom_json_dict(Response, ResponseJSON, []),
    PollID = ResponseJSON.get(result).get(poll).get(id),
    ChatID = ResponseJSON.get(result).get(chat).get(id),
    ( retract(current_poll(_, ChatID)); true ),
    assert(current_poll(PollID, ChatID)),
    !.

process_poll(Message) :-
    PollID = Message.get(poll_answer).get(poll_id),
    current_poll(PollID, ChatID),
    User = Message.get(poll_answer).get(user).get(username),
    string_concat('@', User, Username),
    AnswerIDs = Message.get(poll_answer).get(option_ids),
    process_smoker(AnswerIDs, Username, ChatID).

process_smoker([AnswerID|_], User, ChatID) :-
    office(AnswerID, Office),
    \+ Office = "None",
    ( retract(smoker(User, _, ChatID));
    true),
    assert(smoker(User, Office, ChatID)),
    save_smokers,
    atomics_to_string(["Add", User, Office, ChatID], " ", Log),
    log_print(log_level('INFO'), Log),
    !.

process_smoker([AnswerID|_], User, ChatID) :-
    office(AnswerID, "None"),
    !,
    retract(smoker(User, Office, ChatID)),
    save_smokers,
    atomics_to_string(["Remove", User, Office, ChatID], " ", Log),
    log_print(log_level('INFO'), Log).

process_message(Message) :-
    _ = Message.get(poll_answer),
    !,
    process_poll(Message).

process_message(Message) :-
    _ = Message.get(message).get(forward_from),
    !.

process_message(Message) :-
    get_smokers_on_question(Message, Usernames),
    list_to_set(Usernames, UsernamesUniq),
    \+ UsernamesUniq = [],
    ping(Message, UsernamesUniq),
    !.

process_message(_).

main :-
    send_poll,
    log_print(log_level('INFO'), "Started"),
    fail.

main :-
    repeat,
    get_updates(Data),
    Messages = Data.get(result),
    last(Messages, LastMessage),
    update_offset(LastMessage),
    maplist(process_message, Messages),
    fail.

?- main.
