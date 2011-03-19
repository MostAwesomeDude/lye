from pymeta.grammar import OMeta

grammar = """

lilypond ::= (<token "\\invalid">
    | <toplevel_expression> | <assignment> | <error>)*

toplevel_expression ::= <lilypond_header>
    | <book_block>
    | <bookpart_block>
    | <score_block>
    | <composite_music>
    | <full_markup>
    | <full_markup_list>
    | <output_def>

embedded_scm ::= <SCM_TOKEN> | <SCM_IDENTIFIER>

lilypond_header_body ::= <token "">
    | <lilypond_header_body> <assignment>

lilypond_header ::=
    <token "\\header"> <token "{"> <lilypond_header_body>:lhb <token "{">
    => lhb

assignment_id ::= <STRING>
    | <LYRICS_STRING>

assignment ::= <assignment_id> <token "="> <identifier_init>
    | <assignment_id> <property_path> <token "="> <identifier_init>
    | <embedded_scm>

identifier_init ::= <score_block>
    | <book_block>
    | <bookpart_block>
    | <output_def>
    | <context_def_spec_block>
    | <music>
    | <post_event>
    | <number_expression>
    | <string>
    | <embedded_scm>
    | <full_markup>
    | <full_markup_list>
    | <DIGIT>
    | <context_modification>

context_def_spec_block ::=
    <token "\\context"> <token "{"> <context_def_spec_body>:cdsb <token "}">
    => cdsb

context_def_spec_body ::= <token "">
    | <CONTEXT_DEF_IDENTIFIER>
    | <context_def_spec_body> <token "\\grobdescriptions"> <embedded_scm>
    | <context_def_spec_body> <context_mod>
    | <context_def_spec_body> <context_modification>

book_block ::= <token "\\book"> <token "{"> <book_body>:bb <token "}">
    => bb

book_body ::= <token "">
    | <BOOK_IDENTIFIER>
    | <book_body> <paper_block>
    | <book_body> <bookpart_block>
    | <book_body> <score_block>
    | <book_body> <composite_music>
    | <book_body> <full_markup>
    | <book_body> <full_markup_list>
    | <book_body> <lilypond_header>
    | <book_body> <error>

bookpart_block ::=
    <token "\\bookpart"> <token "{"> <bookpart_body>:bb <token "}">
    => bb

bookpart_body ::= <token "">
    | <BOOK_IDENTIFIER>
    | <bookpart_body> <paper_block>
    | <bookpart_body> <score_block>
    | <bookpart_body> <composite_music>
    | <bookpart_body> <full_markup>
    | <bookpart_body> <full_markup_list>
    | <bookpart_body> <lilypond_header>
    | <bookpart_body> <error>

score_block ::= <token "\\score"> <token "{"> <score_body>:sb <token "}">
    => sb

score_body ::= <music>
    | <SCORE_IDENTIFIER>
    | <score_body> <lilypond_header>
    | <score_body> <output_def>
    | <score_body> <error>

paper_block ::= <output_def>

output_def ::= <output_def_body> <token "}">

output_def_head ::= <token "\\paper">
    | <token "\\midi">
    | <token "\\layout">

output_def_body ::= <output_def_head> <token "{">
    | <output_def_head> <token "{"> <OUTPUT_DEF_IDENTIFIER>
    | <output_def_body> <assignment>
    | <output_def_body> <context_def_spec_block>
    | <output_def_body> <error>

tempo_event ::= <token "\\tempo"> <steno_duration> <token "="> <tempo_range>
    | <token "\\tempo"> <scalar> <steno_duration> <token "="> <tempo_range>
    | <token "\\tempo"> <scalar>

music_list ::= <token "">
    | <music_list> <music>
    | <music_list> <embedded_scm>
    | <music_list> <error>

music ::= <simple_music>
    | <composite_music>

alternative_music ::= <token "">
    | <token "\\alternative"> <token "{"> <music_list>:ml <token "}">
    => ml

repeated_music ::= <token "\\repeat">
    | <simple_string>
    | <unsigned_number>
    | <music>
    | <alternative_music>

sequential_music ::=
    | <token "\\sequential"> <token "{"> <music_list>:ml <token "}">
    | <token "{"> <music_list>:ml <token "}">
    => ml

simultaneous_music ::=
    | <token "\\simultaneous"> <token "{"> <music_list>:ml <token "}">
    | <token "<<"> <music_list>:ml <token ">>">
    => ml

simple_music ::= <event_chord>
    | <MUSIC_IDENTIFIER>
    | <music_property_def>
    | <context_change>

context_modification ::=
    <token "\\with"> <token "$"> <token "{"> <context_mod_list> <token "}">
    | <token "\\with"> <CONTEXT_MOD_IDENTIFIER>
    | <CONTEXT_MOD_IDENTIFIER>

optional_context_mod ::= <token ""> | <context_modification>

context_mod_list ::= <token "">
    | <context_mod_list> <context_mod>
    | <context_mod_list> <CONTEXT_MOD_IDENTIFIER>

composite_music ::= <prefix_composite_music>
    | <grouped_music_list>

grouped_music_list ::= <simultaneous_music>
    | <sequential_music>

function_scm_argument ::= <embedded_scm>
    | <simple_string>

function_arglist_music_last ::= <EXPECT_MUSIC> <function_arglist> <music>

function_arglist_nonmusic_last ::=
    <EXPECT_MARKUP> <function_arglist> <full_markup>
    | <EXPECT_MARKUP> <function_arglist> <simple_string>
    | <EXPECT_SCM> <function_arglist> <function_scm_argument>

function_arglist_nonmusic ::= <EXPECT_NO_MORE_ARGS>
    | <EXPECT_MARKUP> <function_arglist_nonmusic> <full_markup>
    | <EXPECT_MARKUP> <function_arglist_nonmusic> <simple_string>
    | <EXPECT_SCM> <function_arglist_nonmusic> <function_scm_argument>

function_arglist ::= <EXPECT_NO_MORE_ARGS>
    | <function_arglist_music_last>
    | <function_arglist_nonmusic_last>

generic_prefix_music_scm ::= <MUSIC_FUNCTION> <function_arglist>

optional_id ::= <token "="> <simple_string>:ss => ss

prefix_composste_music ::= <generic_prefix_music_scm>
    | <token "\\context"> <simple_string> <optional_id> <optional_context_mod>
        <music>
    | <token "\\new"> <simple_string> <optional_id> <optional_context_mod>
        <music>
    | <token "\\times"> <fraction> <music>
    | <repeated_music>
    | <token "\\transpose"> <pitch_also_in_chords> <pitch_also_in_chords>
        <music>
    | <mode_changing_head> <grouped_music_list>
    | <mode_changing_head_with_context> <optional_context_mod>
        <grouped_music_list>
    | <relative_music>
    | <re_rhythmed_music>

mode_changing_head ::= <token "\\notemode">
    | <token "\\drummode">
    | <token "\\figuremode">
    | <token "\\chordmode">
    | <token "\\lyricmode">

mode_changing_head_with_context ::= <token "\\drums">
    | <token "\\figures">
    | <token "\\chords">
    | <token "\\lyrics">

relative_music ::= <token "\\relative"> <composite_music>
    | <token "\\relative"> <absolute_pitch> <music>

new_lyrics ::= <token "\\addlyrics">

re_rhythmed_music ::= <grouped_music_list> <new_lyrics>
    | <MUSIC_IDENTIFIER> <new_lyrics>

context_change ::= <token "\\change"> <STRING> <token "="> <STRING>

property_path_revved ::= <embedded_scm>
    | <property_path_revved> <embedded_scm>

property_path ::= <property_path_revved>

property_operation ::= <STRING> <token "="> <scalar>
    | <token "\\unset"> <simple_string>
    | <token "\\override"> <simple_string> <property_path> <token "=">
        <scalar>
    | <token "\\revert"> <simple_string> <embedded_scm>

context_def_mod ::= <token "\\consists">
    | <token "\\remove">
    | <token "\\accepts">
    | <token "\\defaultchild">
    | <token "\\denies">
    | <token "\\alias">
    | <token "\\type">
    | <token "\\description">
    | <token "\\name">

context_mod ::= <property_operation>
    | <context_def_mod> <STRING>
    | <context_def_mod> <embedded_scm>

context_prop_spec ::= <simple_string> (<token "."> <simple_string>)?

simple_music_property_def ::=
    <token "\\override"> <context_prop_spec> <property_path> <token "=">
        <scalar>
    | <token "\\revert"> <context_prop_spec> <embedded_scm>
    | <token "\\set"> <context_prop_spec> <token "="> <scalar>
    | <token "\\unset"> <context_prop_spec>

music_property_def ::= <token "\\once">? <simple_music_property_def>

string ::= <STRING>
    | <STRING_IDENTIFIER>
    | <string> <token "+"> <string>

simple_string ::= <STRING>
    | <LYRICS_STRING>
    | <STRING_IDENTIFIER>

scalar ::= <string>
    | <LYRICS_STRING>
    | <bare_number>
    | <embedded_scm>
    | <full_markup>
    | <DIGIT>

event_chord ::= <simple_chord_elements> <post_events>
    | <CHORD_REPETITION> <optional_notemode_duration> <post_events>
    | <MULTI_MEASURE_REST> <optional_notemode_duration> <post_events>
    | <command_element>
    | <note_chord_element>

note_chord_element ::=
    <chord_body> <optional_notemode_duration> <post_events>

chord_body ::= <token "<"> <chord_body_elements> <token ">">

chord_body_elements ::= <token "">
    | <chord_body_elements> <chord_body_element>

chord_body_element ::=
    <pitch> <exclamations> <questions> <octave_check> <post_events>
    | <DRUM_PITCH> <post_events>
    | <music_function_chord_body>

music_function_identifier_musicless_prefix ::= <MUSIC_FUNCTION>

music_function_chord_body ::=
    <music_function_identifier_musicless_prefix> <EXPECT_MUSIC>
        <function_arglist_nonmusic> <chord_body_element>
    | <music_function_identifier_musicless_prefix> <function_arglist_nonmusic>

music_function_event ::=
    <music_function_identifier_musicless_prefix> <EXPECT_MUSIC>
        <function_arglist_nonmusic> <post_event>
    | <music_function_identifier_musicless_prefix> <function_arglist_nonmusic>

command_element ::= <command_event>
    | <token "\\skip"> <duration_length>
    | <token "\\[">
    | <token "\\]">
    | <token "\\">
    | <token "|">
    | <token "\\partial"> <duration_length>
    | <token "\\time"> <fraction>
    | <token "\\mark"> <scalar>

command_event ::= <token "\\~">
    | <token "\\mark"> <token "\\default">
    | <tempo_event>
    | <token "\\key"> <token "\\default">
    | <token "\\key"> <NOTENAME_PITCH> <SCM_IDENTIFIER>

"""
"""
Port of the Lilypond grammar to PyMeta. This is based on the 2.13 Ly grammar.
"""

class LyeGrammar(OMeta.makeGrammar(grammar, globals())):
    pass
