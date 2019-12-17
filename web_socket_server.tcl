

#		Euchre App
#	Tommy Freethy - July 2017
#
# This script is the entire back end for a Euchre web application. It takes care of all incoming
# HTTP requests, upgrades to websockets when necessary, and controls all aspects of the game itself.
#
# The app allows for multiple games lobbies with 1-4 human players per lobby, then fills the empty
# seats with bots. The euchre bots are NOT to be under estimated!
#


package require websocket
package require itcl
package require tls
package require json

namespace eval web_server {
	variable _server_socket ""
	variable _pending_player_sockets ""
	# variable _source_path "c:/tbf/Test/euchre"
	variable _source_path "/home/ec2-user/tbf/euchre"
	variable _euchre_scores_path [file join $_source_path euchre_scores.txt]
	variable _https_enabled 1
	
	proc main {} {
		
		# setup_euchre_scores_file
		
		setup_web_server
		vwait forever
	}
	
# ----------------------------------------Web Server--------------------------------------------	
	
	# This procedure sets up the listening socket for the web server. If https is enabled, a second socket
	# will be opened on port 443 - this requires valid SSL certificates.
	proc setup_web_server {} {
		variable _server_socket
		variable _source_path
		variable _https_enabled
		
		set _server_socket [socket -server [namespace code connection_accept] 80]
		websocket::server $_server_socket
		websocket::live $_server_socket / [namespace code wsLiveTest]
		puts "main...http websocket live"
		
		if {$_https_enabled} {
			set Ciphers ""
			# Filter ciphers
			foreach Cipher [tls::ciphers tls1.2] {
				switch -glob -- $Cipher {
					"DHE-*" -
					"DH-*" -
					"*-MDA5*" -
					"*RC4*" -
					"*IDEA*" -
					"*DES*" {
						continue
					}
					default {
						lappend Ciphers "$Cipher"
					}
				}
			}
			
			set _server_socket_tls [tls::socket -server [namespace code connection_accept_https] \
				-certfile [file join $_source_path ssl certificate.crt] \
				-keyfile [file join $_source_path ssl private.key] \
				-cafile [file join $_source_path ssl ca_bundle.crt] \
				-ssl2 0 \
				-ssl3 0 \
				-tls1 0 \
				-tls1.1 0 \
				-tls1.2 1 \
				-request 0 \
				-require 0 \
				-cipher "$Ciphers" \
				443 \
			]
			websocket::server $_server_socket_tls
			websocket::live $_server_socket_tls / [namespace code wsLiveTest]
			puts "main...https websocket live"
		}
	}
	
	# Little "database" to keep track of how dominant the bots are.
	proc setup_euchre_scores_file {} {
		variable _euchre_scores_path
		if {[file exists $_euchre_scores_path] == 0} {
			set File [open $_euchre_scores_path "w"]
			
			puts $File "bot_with_bot_games 0"
			puts $File "bot_with_bot_wins 0"
			puts $File "bot_with_bot_points_for 0"
			puts $File "bot_with_bot_points_against 0"
			puts $File "bot_with_human_games 0"
			puts $File "bot_with_human_wins 0"
			puts $File "bot_with_human_points_for 0"
			puts $File "bot_with_human_points_against 0"
			puts $File "human_with_human_games 0"
			puts $File "human_with_human_wins 0"
			puts $File "human_with_human_points_for 0"
			puts $File "human_with_human_points_against 0"
			
			close $File
		}
	}
	
	# Accepts any new HTTPS connection
	proc connection_accept_https {Socket IP Port} {
		if {[catch {tls::handshake $Socket} Error]} {
			puts "handshake error...$Error"
			close $Socket
			return
		}
		# tls::handshake $Socket
		
		puts "connection_accept_https...IP=$IP, [clock format [clock seconds] -format {%Y:%m:%d %H:%M:%S}]"
		set ConnectionObject [http_connection "connection_$Socket" $Socket]
		
		fconfigure $Socket -blocking 0 -buffering none -translation binary
		fileevent $Socket readable [namespace code [list http_read $ConnectionObject]]
	}
	
	# Accepts any new HTTP connection
	proc connection_accept {Socket IP Port} {
		set ConnectionObject [http_connection "connection_$Socket" $Socket 1]
		
		fconfigure $Socket -blocking 0 -buffering line
		fileevent $Socket readable [namespace code [list http_read $ConnectionObject]]
	}
	
	# Reads http data from the socket one line at a time
	proc http_read {ConnectionObject} {
		variable _server_socket
		
		set Socket [$ConnectionObject var_get socket]
		array set HttpHeader [$ConnectionObject array_get http_header_in]
		
		set GetsReturn [gets $Socket Line]
		# puts "$Socket...GetsReturn=$GetsReturn, Line=$Line"
		
		# This means we found the empty line before the payload.
		if {$Line eq "\r"} {
			set GetsReturn 0
		}
		
		# Set up the first couple elements in HTTP header with the first line of request
		if {[array names HttpHeader] eq ""} {
			set HttpHeader(proto) [lindex $Line 0]
			set HttpHeader(url) [lindex $Line 1]
			set HttpHeader(query) [lindex $Line 2]
			
			set HttpHeader(query_url) ""
			switch [llength [set UrlList [split $HttpHeader(url) "?"]]] {
				1 {
					set HttpHeader(path) [lindex $UrlList 0]
				}
				2 {
					set HttpHeader(path) [lindex $UrlList 0]
					set HttpHeader(query_url) [lindex $UrlList 1]
				}
			}
		}
		switch -glob -- [string compare $GetsReturn 0],$HttpHeader(proto) {
			1,GET -
			1,POST {
				# Here we have read a line of the http header. Parse line and set element in http array
				set NameValue [split $Line ":"]
				set Name [string trim [lindex $NameValue 0]]
				set Value [string trim [lindex $NameValue 1]]
				set HttpHeader($Name) $Value
				$ConnectionObject array_set http_header_in [array get HttpHeader]
			}
			0,GET {
				# Here we have a GET request and have finished reading the http request
				# We are checking for the "Upgrade" header to read "websocket", if so we upgrade the socket
				# Then we handle the get request
				
				set HeadersList [list]
				if {[array names HttpHeader "Upgrade"] ne ""} {
					if {[string tolower $HttpHeader(Upgrade)] eq "websocket"} {
						puts "Upgrading to Web Socket"
						foreach {Name Value} [array get HttpHeader] {
							lappend HeadersList $Name $Value
						}
						set TestResult [websocket::test $_server_socket $Socket / $HeadersList]
						if {$TestResult} {
							websocket::upgrade $Socket
							$ConnectionObject var_set is_web_socket 1
						}
						puts "done upgrading socket" 
						return
					}
				}
				handle_request $ConnectionObject
			}
			0,POST {
				# Here we have a POST request and have to read in the request payload. The exact content
				# length must be read from the socket. The query is then decoded, then the request is handled.
				set QueryDataString [read $Socket $HttpHeader(Content-Length)]
				if {[array names -exact "Content-Type"] ne "" && \
					$HttpHeader(Content-Type) eq "application/json" \
				} {
					set QueryDict [json::json2dict $QueryDataString]
					array unset query_url
					foreach Key [dict keys $QueryDict] {
						set query_url($Key) [dict get $QueryDict $Key]
					}
				} else {
					array set query_data [decode_query $QueryDataString]
					array set query_url [decode_query $HttpHeader(query_url)]
					foreach {Name Value} [array get query_data] {
						set query_url($Name) $Value
					}
				}
				$ConnectionObject array_set query [array get query_url]
				handle_request $ConnectionObject
			}
			-1,* {
				# puts "bad read, closing socket $Socket"
				itcl::delete object $ConnectionObject
			}
		}
	}
	
	# The next 2 procs decode form encoded queries
	proc decode_query {QueryString} {
		::foreach Pair [::split $QueryString "&"] {
			::foreach {Name Value} [::split $Pair "="] {
				::set Name [Url_Decode $Name];
				::set Value [Url_Decode $Value];
				::set Query($Name) $Value;
			}
		}
		::return [::array get Query]
	}
	proc Url_Decode {data} {
		regsub -all {\+} $data " " data
		regsub -all {([][$\\])} $data {\\\1} data
		regsub -all {%([0-9a-fA-F][0-9a-fA-F])} $data  {[format %c 0x\1]} data
		return [subst $data]
	}
	
	# This proc sends the response to an http request. 
	proc respond {ConnectionObject Code Body} {
		set Socket [$ConnectionObject var_get socket]
		array set HttpHeader [$ConnectionObject array_get http_header_out]
		
		set Text ""
		switch $Code {
			200 {set Text "OK"}
			201 {set Text "Created"}
			202 {set Text "Accepted"}
			301 {set Text "Moved Permanently"}
			302 {set Text "Found"}
			304 {set Text "Not Modified"}
			401 {set Text "Unauthorized"}
			403 {set Text "Forbidden"}
			404 {set Text "Not Found"}
		}
		
		puts $Socket "HTTP/1.1 $Code $Text"
		foreach {Name Value} [array get HttpHeader] {
			puts $Socket "$Name: $Value"
		}
		puts $Socket ""
		if {[array names -exact "Content-Type"] ne "" && \
			[string first "image" $HttpHeader(Content-Type)] >= 0 \
		} {
			fconfigure $Socket -translation "binary";
		}
		puts $Socket "$Body"
		
		itcl::delete object $ConnectionObject
    }
	
	# Reads file
	proc read_file {FilePath {Translation ""}} {
		if {[catch {open $FilePath} FileHandle]} {
			puts "Could not read file: $FilePath"
			return "Could not read file: $FilePath"
		}
		if {$Translation ne ""}  {
			fconfigure $FileHandle -translation $Translation
		}
		set Content [read $FileHandle]
		close $FileHandle
		return $Content
	}
	
	# This proc handles all the http requests. In this case it will be all GET requests, for the HTML,
	# CSS, JavaScript, and other resources of the app.
	proc handle_request {ConnectionObject} {
		variable _source_path
		variable _cookie_array
		variable _https_enabled
		
		set Socket [$ConnectionObject var_get socket]
		array set HttpHeader [$ConnectionObject array_get http_header_in]
		array set Query [$ConnectionObject array_get query]
		
		if {$_https_enabled && [$ConnectionObject var_get upgrade_to_https]} {
			if {[array names HttpHeader Host] eq ""} {
				respond $ConnectionObject 403 ""
				return
			}
			$ConnectionObject set_http_header_out [list \
				"Location" "https://$HttpHeader(Host)$HttpHeader(path)"
			]
			respond $ConnectionObject 302 "Redirect to HTTPS"
			return
		}
		
		# puts "$HttpHeader(proto) request on url: $HttpHeader(path)"
		switch -glob -- $HttpHeader(path) {
			"/.well-known*" {
				set FilePath [lindex [split $HttpHeader(path) "/"] end]
				set Data [read_file ${_source_path}/$FilePath]
				respond $ConnectionObject 200 "$Data"
			}
			"*.css" {
				set CSS [read_file "$_source_path$HttpHeader(path)"]
				
				$ConnectionObject set_http_header_out [list "Content-Type" "text/css"]
				respond $ConnectionObject 200 "$CSS"
			}
			"*.js" {
                set JS [read_file "$_source_path$HttpHeader(path)"]
				
				$ConnectionObject set_http_header_out [list "Content-Type" "text/javascript"]
				respond $ConnectionObject 200 "$JS"
			}
			"apple-touch-icon*" {
				set Data [read_file "${_source_path}/spade.png" "binary"]
				$ConnectionObject set_http_header_out [list "Content-Type" "image/png"]
				respond $ConnectionObject 200 "$Data"
			}
			"*spade.png" {
                set Data [read_file "$_source_path$HttpHeader(path)" "binary"]
				
				$ConnectionObject set_http_header_out [list "Content-Type" "image/png"]
				respond $ConnectionObject 200 "$Data"
			}
			"*.json" {
                set JSON [read_file "$_source_path$HttpHeader(path)"]
				
				$ConnectionObject set_http_header_out [list "Content-Type" "text/json"]
				respond $ConnectionObject 200 "$JSON"
			}
            default {
				set HTML [read_file "${_source_path}/index.html"]
				respond $ConnectionObject 200 $HTML
            }
		}
    }
	
	# This is where all the websocket traffic comes in. Each text message is a JSON object with an "action"
	# field to describe the purpose of the message.
	#
	# The main flow of the game is found in here. Upon each action we check to see whether the next player
	# to play is a bot. If so, the bot makes its highly calculated move and the action is broadcasted to the 
	# other players.
	#
	# A common strategy used below is to simulate a message to the web server. i.e. call this proc as if
	# it were an incoming message. Here is an example copied from the "START_GAME" action:
	# 		wsLiveTest $ClientSocket "text" \{"action":"START_HAND"\} $args
	# This strategy is used when bots make moves. The above statement is wrapped in an "after 1000"
	# to introduce a pause to make game play smooth.
	proc wsLiveTest {ClientSocket Operation Par args} {
		variable _pending_player_sockets
		
		# Clean up _pending_player_sockets
		set PlayerObject $ClientSocket
		if {[itcl::find objects $PlayerObject] eq ""} {
			set PlayerObject "connection_$ClientSocket"
			if {[itcl::find objects $PlayerObject] eq ""} {
				set PlayerSocketIndex [lsearch $_pending_player_sockets $ClientSocket]
				if {$PlayerSocketIndex >= 0} {
					set _pending_player_sockets [lreplace $_pending_player_sockets $PlayerSocketIndex $PlayerSocketIndex]
				}
				return
			}
		}
		
# ----------------------------------------Game Flow--------------------------------------------	
		
		# This is in the middle of a proc, but this is where the game logic starts.
		
		switch $Operation {
			"close" -
			"disconnect" {
				# When a player disconnects we replace them with a bot.
				
				if {[itcl::find objects $PlayerObject] eq ""} {return}
				
				set PlayerSocketIndex [lsearch $_pending_player_sockets $ClientSocket]
				if {$PlayerSocketIndex >= 0} {
					set _pending_player_sockets [lreplace $_pending_player_sockets $PlayerSocketIndex $PlayerSocketIndex]
				}
				
				set Lobby [$PlayerObject var_get euchre_lobby]
				if {[itcl::find objects $Lobby] ne ""} {
					# puts "disconnect/close...players_list=[$Lobby var_get players_list]"
					if {[llength [$Lobby var_get players_list]] == 1} {
						itcl::delete object $Lobby
					} else {
						$Lobby remove_from_lobby $ClientSocket
						set Name [lindex [split "$Lobby" "_"] 1]
						set Name "[string map [list %= " "] $Name]"
						set JSON [write_JSON [subst {
							players {[get_JSON_players $Lobby]}
							action SIT_DOWN
							lobbyName $Name
						}]]
						foreach PlayerSocket [$Lobby var_get players_list] {
							websocket::send $PlayerSocket "text" $JSON
						}
						
						set LastCommand [$Lobby var_get last_command]
						if {$LastCommand eq ""} {return}
						set Par [lindex $LastCommand 3]
						set QueryDict [json::json2dict $Par]
						set Action [dict get $QueryDict "action"]
						set SeatNumber 0
						if {[dict exists $QueryDict "seatNum"]} {
							set SeatNumber [dict get $QueryDict "seatNum"]
						}
						switch $Action {
							"START_TRICK" -
							"START_HAND" {
								set SeatNumber [$Lobby var_get dealer]
							}
							"SIT_DOWN" {
								return
							}
						}
						set Seat [$PlayerObject var_get seat_number]
						set Hand [$PlayerObject var_get euchre_hand]
						
						set BotHandleName "${Lobby}_bot_${Seat}"
						euchre_bot $BotHandleName
						$BotHandleName var_set euchre_lobby $Lobby
						$BotHandleName var_set seat_number $Seat
						$BotHandleName var_set nickname "Bot_$Seat"
						$BotHandleName var_set euchre_hand $Hand
						$Lobby append_bots_list $BotHandleName
						
						set JSON [write_JSON [subst {
							players {[get_JSON_players $Lobby]}
							action SIT_DOWN
							lobbyName $Name
						}]]
						foreach PlayerSocket [$Lobby var_get players_list] {
							websocket::send $PlayerSocket "text" $JSON
						}
						
						set SeatNumber [expr {int(fmod($SeatNumber, 4)) + 1}]
						if {$SeatNumber == $Seat} {
							eval $LastCommand
						}
					}
				}
				set JSON [write_JSON [subst {
					lobsters {[get_JSON_lobbies]}
					action UPDATE_LOBBIES
				}]]
				foreach PendingPlayer $_pending_player_sockets {
					websocket::send $PendingPlayer "text" $JSON
				}
				itcl::delete object $PlayerObject
			}
			"text" {
				set QueryDict [json::json2dict $Par]
				# puts "Received Action: [dict get $QueryDict action], $Par"
				
				set EuchreLobbyTemp [$PlayerObject var_get euchre_lobby]
				if {$EuchreLobbyTemp ne ""} {
					$EuchreLobbyTemp var_set last_command [list wsLiveTest $ClientSocket $Operation $Par $args] 
				}
				
				switch -glob -- [dict get $QueryDict "action"] {
					"CREATE_LOBBY" {
						# Someone has created a new euchre lobby. Create an object named with the name of the
						# lobby. If it exists send an error back to the user. Otherwise broadcast to all pending
						# players.
						
						set Name "[dict get $QueryDict lobbyName]"
						set Name [string map [list " " "%="] $Name]
						set EuchreLobbyName "euchre_$Name"
						if {[itcl::find objects $EuchreLobbyName -class euchre_lobby] ne ""} {
							set JSON [write_JSON {
								action CREATE_LOBBY
								result failed
								error "Lobby already exists"
							}]
							websocket::send $ClientSocket "text" $JSON
							return
						}
						set EuchreLobby [euchre_lobby $EuchreLobbyName]
						
						set JSON [write_JSON {
							action CREATE_LOBBY
							result success
						}]
						websocket::send $ClientSocket "text" $JSON
						
						set JSON [write_JSON [subst {
							lobsters {[get_JSON_lobbies]}
							action UPDATE_LOBBIES
						}]]
						foreach PendingPlayer $_pending_player_sockets {
							websocket::send $PendingPlayer "text" $JSON
						}
					}
					"LOG_IN" {
						# New player has entered the pending players lobby. Give that user a list of current euchre lobbies.
						
						puts "New Player: [dict get $QueryDict name]"
						$PlayerObject var_set nickname "[dict get $QueryDict name]"
						
						lappend _pending_player_sockets $ClientSocket
						
						set JSON [write_JSON [subst {
							lobsters {[get_JSON_lobbies]}
							action LOG_IN
						}]]
						websocket::send $ClientSocket "text" $JSON
					}
					"SIT_DOWN" {
						# When the server receives this message, a new player or bot has entered the euchre lobby.
						# The objects are updated on the server, then all member of the game are notified of the
						# change in players. Then pending players are updated to see the change in that lobby.
						
						$PlayerObject var_set seat_number [dict get $QueryDict "seatNum"]
						set Name [dict get $QueryDict "lobbyName"]
						set Name [string map [list " " "%="] $Name]
						set EuchreLobby "euchre_$Name"
						
						set PlayerSocketIndex [lsearch $_pending_player_sockets $ClientSocket]
						if {$PlayerSocketIndex >= 0} {
							set _pending_player_sockets [lreplace $_pending_player_sockets $PlayerSocketIndex $PlayerSocketIndex]
						}
						
						$EuchreLobby append_players_list $ClientSocket
						$PlayerObject var_set euchre_lobby $EuchreLobby
						
						set JSON [write_JSON [subst {
							players {[get_JSON_players $EuchreLobby]}
							action SIT_DOWN
						}]]
						foreach PlayerSocket [$EuchreLobby var_get players_list] {
							websocket::send $PlayerSocket "text" $JSON
						}
						
						set JSON [write_JSON [subst {
							lobsters {[get_JSON_lobbies]}
							action UPDATE_LOBBIES
						}]]
						foreach PendingPlayer $_pending_player_sockets {
							websocket::send $PendingPlayer "text" $JSON
						}
					}
					"STAND_UP" {
						# A player has left the euchre lobby and is back in the pending players lobby.
						
						$PlayerObject var_set seat_number ""
						set EuchreLobby [$PlayerObject var_get euchre_lobby]
						$EuchreLobby remove_from_lobby [$PlayerObject var_get socket]
						
						set JSON [write_JSON [subst {
							players {[get_JSON_players $EuchreLobby]}
							action SIT_DOWN
						}]]
						
						lappend _pending_player_sockets [$PlayerObject var_get socket]
						
						# If no human players remain, delete the lobby, otherwise update the remaining players in the lobby.
						if {[llength [$EuchreLobby var_get players_list]] == 0} {
							itcl::delete object $EuchreLobby
						} else {
							foreach PlayerSocket [$EuchreLobby var_get players_list] {
								websocket::send $PlayerSocket "text" $JSON
							}
						}
						
						# Update the pending players of the new empty seat.
						set JSON [write_JSON [subst {
							lobsters {[get_JSON_lobbies]}
							action UPDATE_LOBBIES
						}]]
						foreach PendingPlayer $_pending_player_sockets {
							websocket::send $PendingPlayer "text" $JSON
						}
					}
					"START_GAME" {
						# The euchre lobby leader clicked start game.
						
						# Some setup
						set EuchreLobby [$PlayerObject var_get euchre_lobby]
						set Dealer [expr {int(rand() * 4 + 1)}]
						# puts "setting dealer to seat # $Dealer"
						$EuchreLobby var_set dealer $Dealer
						$EuchreLobby var_set hand_counter 0
						
						# Fill empty seats with bots, and some initialization
						set NonEmptySeats ""
						foreach Player [concat [$EuchreLobby var_get players_list] [$EuchreLobby var_get bots_list]] {
							if {[itcl::find objects $Player] eq ""} {
								set Player "connection_$Player"
							}
							set SeatNumber [$Player var_get seat_number]
							lappend NonEmptySeats $SeatNumber
							$Player var_set hand_score_counter 0
						}
						if {[llength $NonEmptySeats] != 4} {
							foreach Seat {1 2 3 4} {
								if {[lsearch $NonEmptySeats $Seat] == -1} {
									set BotHandleName "${EuchreLobby}_bot_${Seat}"
									euchre_bot $BotHandleName
									$BotHandleName var_set euchre_lobby $EuchreLobby
									$BotHandleName var_set seat_number $Seat
									$BotHandleName var_set nickname "Bot_$Seat"
									$EuchreLobby append_bots_list $BotHandleName
								}
							}
							# Update players in lobby
							set JSON [write_JSON [subst {
								players {[get_JSON_players $EuchreLobby]}
								action SIT_DOWN
							}]]
							foreach PlayerSocket [$EuchreLobby var_get players_list] {
								websocket::send $PlayerSocket "text" $JSON
							}
							
							# Update pending players
							set JSON [write_JSON [subst {
								lobsters {[get_JSON_lobbies]}
								action UPDATE_LOBBIES
							}]]
							foreach PendingPlayer $_pending_player_sockets {
								websocket::send $PendingPlayer "text" $JSON
							}
						}
						
						# Call ourselves to get the first hand started.
						wsLiveTest $ClientSocket "text" \{"action":"START_HAND"\} $args
					}
					"RESTART_GAME" {
						# Lobby leader started new game, re initialize and broadcast to players
						
						set EuchreLobby [$PlayerObject var_get euchre_lobby]
						set JSON [write_JSON [subst {
							action RESTART_GAME
						}]]
						foreach PlayerSocket [$EuchreLobby var_get players_list] {
							websocket::send $PlayerSocket "text" $JSON
						}
						$EuchreLobby re_init
						wsLiveTest $ClientSocket "text" \{"action":"START_GAME"\} $args
					}
					"START_HAND" {
						set EuchreLobby [$PlayerObject var_get euchre_lobby]
						set Shuffled [shuffle_cards]
						
						# Initialize hand
						$EuchreLobby var_set alone "0_0"
						$EuchreLobby set_up_remaining_cards_array
						$EuchreLobby var_set hand_counter [expr {[$EuchreLobby var_get hand_counter]+1}]
						
						set TopCard [lindex $Shuffled end]
						
						# Deal random cards to bots
						foreach EuchreBot [$EuchreLobby var_get bots_list] {
							set Hand [lrange $Shuffled 0 4]
							set Shuffled [lreplace $Shuffled 0 4]
							$EuchreBot var_set euchre_hand $Hand
							
							set HandScore [$EuchreBot rate_hand $Hand]
							puts "[$EuchreBot var_get nickname]'s hand = $Hand...$HandScore"
							$EuchreBot var_set hand_score_counter [expr {[$EuchreBot var_get hand_score_counter]+$HandScore}]
						}
						
						# Deal random cards to players
						foreach PlayerSocket [$EuchreLobby var_get players_list] {
							set PlayerObject "connection_$PlayerSocket"
							set Hand [lrange $Shuffled 0 4]
							set Shuffled [lreplace $Shuffled 0 4]
							$PlayerObject var_set euchre_hand $Hand
							
							set HandScore [$EuchreBot rate_hand $Hand]
							puts "[$PlayerObject var_get nickname]'s hand = $Hand...$HandScore"
							$PlayerObject var_set hand_score_counter [expr {[$PlayerObject var_get hand_score_counter]+$HandScore}]
							
							# Broadcast to players that the cards are dealt and hand has started
							set JSON [write_JSON [subst {
								action START_HAND
								dealer [$EuchreLobby var_get dealer]
								hand [get_JSON_cards $Hand]
								topCard [get_JSON_cards $TopCard]
							}]]
							
							websocket::send $PlayerSocket "text" $JSON
						}
						$EuchreLobby var_set top_card $TopCard
						$EuchreLobby var_set top_card_suit [lindex [split $TopCard "_"] 1]
						
						# Check to see if a bot has the first move
						foreach EuchreBot [$EuchreLobby var_get bots_list] {
							set Lead [expr {int(fmod([$EuchreLobby var_get dealer], 4)) + 1}]
							set SeatNumber [$EuchreBot var_get seat_number]
							if {$SeatNumber == $Lead} {
								# Test hand to determine whether to order up the dealer
								# Then broadcast the pass or order_upp to the players
								set TopCardSuit [lindex [split $TopCard "_"] 1]
								set TrumpScore [$EuchreBot call_trump_this_suit $TopCardSuit]
								if {$TrumpScore > 11} {
									set IsAlone "false"
									if {$TrumpScore >= 25} {
										set IsAlone "true"
									}
									set JSON [write_JSON [subst {
										action ORDER_UP
										seatNum $SeatNumber
										isAlone $IsAlone
									}]]
									after 1000 [namespace code [list wsLiveTest $EuchreBot "text" $JSON $args]]
									break
								} else {
									set JSON [write_JSON [subst {
										action PASS
										seatNum $SeatNumber
									}]]
									after 1000 [namespace code [list wsLiveTest $EuchreBot "text" $JSON $args]]
									break
								}
							}
						}
					}
					"PASS" {
						# A player has passed on making trump/ordering up the dealer.
						
						set EuchreLobby [$PlayerObject var_get euchre_lobby]
						set SeatNumber [dict get $QueryDict "seatNum"]
						
						set Action "PASS"
						#Dealer passed top card
						if {$SeatNumber == [$EuchreLobby var_get dealer]} {
							set Action "DEALER_PASS"
							$EuchreLobby var_set top_card ""
						}
						
						# Broadcast the pass
						foreach PlayerSocket [$EuchreLobby var_get players_list] {
							set JSON [write_JSON [subst {
								action $Action
								seatNum $SeatNumber
							}]]
							websocket::send $PlayerSocket "text" $JSON
						}
						
						# Check if next move is a bot
						set NextTurn [expr {int(fmod($SeatNumber, 4)) + 1}]
						set TopCard [$EuchreLobby var_get top_card]
						foreach EuchreBot [$EuchreLobby var_get bots_list] {
							set SeatNumber [$EuchreBot var_get seat_number]
							if {$SeatNumber == $NextTurn} {
								if {$TopCard eq ""} {
									# The bots move, top card has been flipped
									
									set BestTrump -25
									set BestSuit ""
									# Find our best suit
									foreach Suit {H D C S} {
										if {$Suit eq [$EuchreLobby var_get top_card_suit]} {
											continue
										}
										set ThisTrumpScore [$EuchreBot rate_hand [$EuchreBot var_get euchre_hand] $Suit]
										if {$ThisTrumpScore > $BestTrump} {
											set BestTrump $ThisTrumpScore
											set BestSuit $Suit
										}
									}
									
									# Stick the dealer or Best hand > 11
									if {$SeatNumber == [$EuchreLobby var_get dealer] || $BestTrump > 11} {
										set IsAlone "false"
										if {$BestTrump >= 25} {
											set IsAlone "true"
										}
										set JSON [write_JSON [subst {
											action CALL_TRUMP
											seatNum $SeatNumber
											trump $BestSuit
											isAlone $IsAlone
										}]]
										after 1000 [namespace code [list wsLiveTest $EuchreBot "text" $JSON $args]]
										return
									}
								} else {
									# The bots move, top card is still up
									
									set TopCardSuit [lindex [split $TopCard "_"] 1]
									
									# Did I deal or am I ordering up someone else?
									if {$SeatNumber == [$EuchreLobby var_get dealer]} {
										set ThisTrumpScore [$EuchreBot dealer_pick_up $TopCard]
									} else {
										set ThisTrumpScore [$EuchreBot rate_hand [$EuchreBot var_get euchre_hand] $TopCardSuit]
									}
									
									# Is the hand goo enough to call trump?
									if {$ThisTrumpScore > 11} {
										set IsAlone "false"
										if {$ThisTrumpScore >= 25} {
											set IsAlone "true"
										}
										set JSON [write_JSON [subst {
											action ORDER_UP
											seatNum $SeatNumber
											isAlone $IsAlone
										}]]
										after 1000 [namespace code [list wsLiveTest $EuchreBot "text" $JSON $args]]
										return
									}
								}
								set JSON [write_JSON [subst {
									action PASS
									seatNum $SeatNumber
								}]]
								after 1000 [namespace code [list wsLiveTest $EuchreBot "text" $JSON $args]]
							}
						}
					}
					"ORDER_UP" {
						# The dealer has been ordered up the top card. Update lobby and broadcast to the
						# players. Check if the dealer is a bot, if so discard.
						
						set EuchreLobby [$PlayerObject var_get euchre_lobby]
						set SeatNumber [dict get $QueryDict "seatNum"]
						set Trump [$EuchreLobby var_get top_card_suit]
						set Alone "0_0"
						if {[dict get $QueryDict "isAlone"] eq "true"} {
							switch $SeatNumber {
								1 -
								3 {set Alone "1_0"}
								2 -
								4 {set Alone "0_1"}
							}
						}
						
						$EuchreLobby var_set trump $Trump
						$EuchreLobby var_set trump_caller $SeatNumber
						$EuchreLobby var_set alone $Alone
						
						set TopCard [$EuchreLobby var_get top_card]
						foreach PlayerSocket [$EuchreLobby var_get players_list] {
							set JSON [write_JSON [subst {
								action ORDER_UP
								seatNum $SeatNumber
								isAlone [dict get $QueryDict "isAlone"]
							}]]
							websocket::send $PlayerSocket "text" $JSON
						}
						foreach EuchreBot [$EuchreLobby var_get bots_list] {
							set EuchreBotSeatNumber [$EuchreBot var_get seat_number]
							if {$EuchreBotSeatNumber == [$EuchreLobby var_get dealer]} {
								set Discard [$EuchreBot dealer_discard $TopCard]
								
								set JSON [write_JSON [subst {
									action DEALER_DISCARD
									card $Discard
								}]]
								
								# Wait 1 second to send this message to the server (self).
								after 1000 [namespace code [list wsLiveTest $EuchreBot "text" $JSON $args]]
							}
						}
					}
					"DEALER_DISCARD" {
						# Update the lobby and start the hand
						
						set EuchreLobby [$PlayerObject var_get euchre_lobby]
						
						set Discard [dict get $QueryDict "card"]
						set TopCard [$EuchreLobby var_get top_card]
						set DealerHand [$PlayerObject var_get euchre_hand]
						set DiscardIndex [lsearch $DealerHand $Discard]
						set DealerHand [lreplace $DealerHand $DiscardIndex $DiscardIndex $TopCard]
						
						$PlayerObject var_set euchre_hand $DealerHand
						$EuchreLobby var_set top_card ""
						
						wsLiveTest $ClientSocket "text" \{"action":"START_TRICK"\} $args
					}
					"START_TRICK" {
						set EuchreLobby [$PlayerObject var_get euchre_lobby]
						# puts "EuchreLobby=$EuchreLobby"
						set Lead [expr {int(fmod([$EuchreLobby var_get dealer], 4)) + 1}]
						# puts "Lead=$Lead"
						
						$EuchreLobby var_set trick_score "0_0"
						
						set JSON [write_JSON [subst {
							action START_TRICK
							lead $Lead
							tricks [write_JSON [subst {
								t1 0
								t2 0
							}]]
						}]]
						foreach PlayerSocket [$EuchreLobby var_get players_list] {
							websocket::send $PlayerSocket "text" $JSON
						}
						
						set Alone [$EuchreLobby var_get alone]
						set TrumpCaller [$EuchreLobby var_get trump_caller]
						set TrumpCallerPartner [expr {int(fmod($TrumpCaller+1, 4)) + 1}]
						# Skip a player's turn if their partner is alone
						if {[string first "1" $Alone] >= 0 && $TrumpCallerPartner == $Lead} {
							set JSON [write_JSON [subst {
								action SKIP_TURN
								seatNum $Lead
							}]]
							foreach PlayerSocket [$EuchreLobby var_get players_list] {
								websocket::send $PlayerSocket "text" $JSON
							}
							set Lead [expr {int(fmod($Lead, 4)) + 1}]
						}
						
						foreach EuchreBot [$EuchreLobby var_get bots_list] {
							set SeatNumber [$EuchreBot var_get seat_number]
							# puts "EuchreBot=$EuchreBot, SeatNumber=$SeatNumber, Lead=$Lead"
							if {$Lead == $SeatNumber} {
								set PlayedCard [$EuchreBot play_card]
								set JSON [write_JSON [subst {
									action PLAY_CARD
									seatNum $SeatNumber
									card $PlayedCard
								}]]
								# Wait 1 second to send the PLAY_CARD action
								after 1000 [namespace code [list wsLiveTest $EuchreBot "text" $JSON $args]]
							}
						}
					}
					"CALL_TRUMP" {
						set EuchreLobby [$PlayerObject var_get euchre_lobby]
						set SeatNumber [dict get $QueryDict "seatNum"]
						set Trump [dict get $QueryDict "trump"]
						set Alone "0_0"
						if {[dict get $QueryDict "isAlone"] eq "true"} {
							switch $SeatNumber {
								1 -
								3 {set Alone "1_0"}
								2 -
								4 {set Alone "0_1"}
							}
						}
						
						$EuchreLobby var_set trump $Trump
						$EuchreLobby var_set trump_caller $SeatNumber
						$EuchreLobby var_set alone $Alone
						
						set JSON [write_JSON [subst {
							action CALL_TRUMP
							seatNum $SeatNumber
							trump $Trump
							isAlone [dict get $QueryDict "isAlone"]
						}]]
						foreach PlayerSocket [$EuchreLobby var_get players_list] {
							websocket::send $PlayerSocket "text" $JSON
						}
						
						wsLiveTest $ClientSocket "text" \{"action":"START_TRICK"\} $args
					}
					"PLAY_CARD" {
						set EuchreLobby [$PlayerObject var_get euchre_lobby]
						set SeatNumber [dict get $QueryDict "seatNum"]
						set Card [dict get $QueryDict "card"]
						
						array set PlayedCards [$EuchreLobby array_get played_cards_array]
						if {[array names PlayedCards $SeatNumber] ne ""} {
							return
						}
						
						# Update lobby object's played cards and player object's hand
						$EuchreLobby set_played_card $SeatNumber $Card
						set PlayerHand [$PlayerObject var_get euchre_hand]
						set CardIndex [lsearch $PlayerHand $Card]
						set PlayerHand [lreplace $PlayerHand $CardIndex $CardIndex]
						$PlayerObject var_set euchre_hand $PlayerHand
						
						# Broadcast action to other players
						set JSON [write_JSON [subst {
							action PLAY_CARD
							seatNum $SeatNumber
							card [get_JSON_cards $Card]
						}]]
						foreach PlayerSocket [$EuchreLobby var_get players_list] {
							websocket::send $PlayerSocket "text" $JSON
						}
						
						# Check if this card played was the lead
						array set PlayedCards [$EuchreLobby array_get played_cards_array]
						if {[llength [array names PlayedCards]] == 1} {
							set Suit [lindex [split $Card "_"] 1]
							if {[lindex [split $Card "_"] 0] eq "J"} {
								set LeftSuit ""
								set Trump [$EuchreLobby var_get trump]
								switch $Trump {
									"H" {set LeftSuit "D"}
									"D" {set LeftSuit "H"}
									"S" {set LeftSuit "C"}
									"C" {set LeftSuit "S"}
								}
								if {$Suit eq $LeftSuit} {
									set Suit $Trump
								}
							}
							$EuchreLobby var_set lead_suit $Suit
						}
						
						# Skip the next player's turn if their partner is alone
						set Alone [$EuchreLobby var_get alone]
						set NextTurn [expr {int(fmod($SeatNumber, 4)) + 1}]
						set TrumpCaller [$EuchreLobby var_get trump_caller]
						set TrumpCallerPartner [expr {int(fmod($TrumpCaller+1, 4)) + 1}]
						if {[string first "1" $Alone] >= 0 && $TrumpCallerPartner == $NextTurn} {
							set JSON [write_JSON [subst {
								action SKIP_TURN
								seatNum $NextTurn
							}]]
							foreach PlayerSocket [$EuchreLobby var_get players_list] {
								websocket::send $PlayerSocket "text" $JSON
							}
							set NextTurn [expr {int(fmod($NextTurn, 4)) + 1}]
						}
						
						# Check if the played card was the last card of the trick
						set BotDelay 1000
						if {[llength [array names PlayedCards]] == 4 || \
							([string first "1" $Alone] >= 0 && [llength [array names PlayedCards]] == 3) \
						} {
							# Determine the winner of the trick and update the lobby's trick_score (score this hand)
							set BotDelay 2500
							set TrickScore [$EuchreLobby var_get trick_score]
							set Team1TrickScore [lindex [split $TrickScore "_"] 0]
							set Team2TrickScore [lindex [split $TrickScore "_"] 1]
							
							set Winner [determine_trick_winner $EuchreLobby]
							switch $Winner {
								1 -
								3 {incr Team1TrickScore}
								2 -
								4 {incr Team2TrickScore}
							}
							set TrickScore "${Team1TrickScore}_${Team2TrickScore}"
							$EuchreLobby var_set trick_score $TrickScore
							
							# Update players on the score of the hand
							set JSON [write_JSON [subst {
								action WIN_TRICK
								winner $Winner
								card $PlayedCards($Winner)
								trickScore [write_JSON [subst {
									t1 $Team1TrickScore
									t2 $Team2TrickScore
								}]]
							}]]
							foreach PlayerSocket [$EuchreLobby var_get players_list] {
								websocket::send $PlayerSocket "text" $JSON
							}
							$EuchreLobby array_unset played_cards_array
							
							# Check if that trick was the last of the hand
							if {[expr {$Team1TrickScore + $Team2TrickScore}] == 5} {
								set GameScore [$EuchreLobby var_get game_score]
								set Team1GameScore [lindex [split $GameScore "_"] 0]
								set Team2GameScore [lindex [split $GameScore "_"] 1]
								
								set Team1Alone [lindex [split $Alone "_"] 0]
								set Team2Alone [lindex [split $Alone "_"] 1]
								
								# Determine how many points are awarded from the hand and update game score
								switch [$EuchreLobby var_get trump_caller] {
									1 -
									3 {
										if {$Team2TrickScore > $Team1TrickScore} {
											incr Team2GameScore 2
										} else {
											if {$Team1TrickScore == 5} {
												if {$Team1Alone == 1} {
													incr Team1GameScore 4
												} else {
													incr Team1GameScore 2
												}
											} else {
												incr Team1GameScore 1
											}
										}
									}
									2 -
									4 {
										if {$Team1TrickScore > $Team2TrickScore} {
											incr Team1GameScore 2
										} else {
											if {$Team2TrickScore == 5} {
												if {$Team2Alone == 1} {
													incr Team2GameScore 4
												} else {
													incr Team2GameScore 2
												}
											} else {
												incr Team2GameScore 1
											}
										}
									}
								}
								set GameScore "${Team1GameScore}_${Team2GameScore}"
								$EuchreLobby var_set game_score $GameScore
								set NewDealer [expr {int(fmod([$EuchreLobby var_get dealer], 4)) + 1}]
								$EuchreLobby var_set dealer $NewDealer
								
								# Broadcast the updated score to the players
								set JSON [write_JSON [subst {
									action WIN_HAND
									gameScore [write_JSON [subst {
										t1 $Team1GameScore
										t2 $Team2GameScore
									}]]
								}]]
								foreach PlayerSocket [$EuchreLobby var_get players_list] {
									after 1000 [namespace code [list websocket::send $PlayerSocket "text" $JSON]]
								}
								
								# Check if either team has 10 or more points, if so they are winners
								if {$Team1GameScore >= 10 || $Team2GameScore >= 10} {
									win_euchre_game $EuchreLobby
									
									# Broadcast to players that the game has ended.
									set JSON [write_JSON [subst {
										action WIN_GAME
										gameScore [write_JSON [subst {
											t1 $Team1GameScore
											t2 $Team2GameScore
										}]]
									}]]
									foreach PlayerSocket [$EuchreLobby var_get players_list] {
										after 1000 [namespace code [list websocket::send $PlayerSocket "text" $JSON]]
									}
									
									# We have been keeping track of player's and bot's average hand score
									foreach Player [concat [$EuchreLobby var_get players_list] [$EuchreLobby var_get bots_list]] {
										if {[itcl::find objects $Player] eq ""} {
											set Player "connection_$Player"
										}
										set HandScore [$Player var_get hand_score_counter]
										set HandCounter [$EuchreLobby var_get hand_counter]
										puts "[$Player var_get nickname] = [expr {double($HandScore) / $HandCounter}]"
									}
									
									# Delete the bots
									foreach Bot [$EuchreLobby var_get bots_list] {
										itcl::delete object $Bot
									}
									$EuchreLobby var_set bots_list ""
									
									# Update players and pending players to show empty seats in this lobby
									set JSON [write_JSON [subst {
										players {[get_JSON_players $EuchreLobby]}
										action SIT_DOWN
									}]]
									foreach PlayerSocket [$EuchreLobby var_get players_list] {
										after 1000 [namespace code [list websocket::send $PlayerSocket "text" $JSON]]
									}
									set JSON [write_JSON [subst {
										lobsters {[get_JSON_lobbies]}
										action UPDATE_LOBBIES
									}]]
									foreach PendingPlayer $_pending_player_sockets {
										websocket::send $PendingPlayer "text" $JSON
									}
									return
								}
								after 3000 [namespace code [list wsLiveTest $ClientSocket "text" \{"action":"START_HAND"\} $args]]
								return
							}
							set NextTurn $Winner
						}
						
						# If the next player is a bot, determine their next move.
						foreach EuchreBot [$EuchreLobby var_get bots_list] {
							set SeatNumber [$EuchreBot var_get seat_number]
							if {$NextTurn == $SeatNumber} {
								set Trump [$EuchreLobby var_get trump]
								set PlayedCard [$EuchreBot play_card]
								
								set JSON [write_JSON [subst {
									action PLAY_CARD
									seatNum $SeatNumber
									card $PlayedCard
								}]]
								after $BotDelay [namespace code [list wsLiveTest $EuchreBot "text" $JSON $args]]
								return
							}
						}
					}
					"CHAT_MESSAGE" {
						set EuchreLobby [$PlayerObject var_get euchre_lobby]
						set Message "[dict get $QueryDict message]"
						set Message [string map [list "\n" "%_bsn"] $Message]
						set Name [dict get $QueryDict "name"]
						
						set JSON [write_JSON [subst {
							message "$Message"
							name "$Name"
							action CHAT_MESSAGE
						}]]
						foreach Player [itcl::find objects "connection_*" -class http_connection] {
							if {[$Player var_get is_web_socket]} {
								set PlayerSocket [$Player var_get socket]
								websocket::send $PlayerSocket "text" $JSON
							}
						}
						
						set Message [string map [list " " "" "_" ""] [string tolower $Message]]
						
						# Remove the 0 from if statement to enable trash talk from bots
						if {0 && $EuchreLobby ne ""} {
							foreach EuchreBot [$EuchreLobby var_get bots_list] {
								set BotName [string tolower [$EuchreBot var_get nickname]]
								set BotName [string map [list "_" "" " " ""] $BotName]
								if {[string first $BotName $Message] >= 0} {
									set Partner [expr {int(fmod([$PlayerObject var_get seat_number]+1, 4)) + 1}]
									
									foreach Player [concat [$EuchreLobby var_get players_list] [$EuchreLobby var_get bots_list]] {
										if {[itcl::find objects $Player] eq ""} {
											set Player "connection_$Player"
										}
										if {[$Player var_get seat_number] == $Partner} {
											set PartnerName [$Player var_get nickname]
										}
									}
									set JSON [write_JSON [subst {
										message "[$EuchreBot get_trash_talk $Name $PartnerName]"
										name [$EuchreBot var_get nickname]
										action CHAT_MESSAGE
									}]]
									foreach PlayerObject [itcl::find objects "connection_*" -class http_connection] {
										if {[$PlayerObject var_get is_web_socket]} {
											set PlayerSocket [$PlayerObject var_get socket]
											websocket::send $PlayerSocket "text" $JSON
										}
									}
								}
							}
						}
					}
					"CRASH_THE_SERVER" {
						websocket::send $PlayerObject "text" "Go Fuck Yourself"
					}
				}
			}
			"error" {
				
			}
		}
	}
	
# ----------------------------------------Game helper procs--------------------------------------------		
	
	# Generate a new shuffled euchre deck
	proc shuffle_cards {} {
		set EuchreDeck ""
		foreach Face {9 10 J Q K A} {
			foreach Suit {C S D H} {
				lappend EuchreDeck "${Face}_${Suit}"
			}
		}
		
		set Shuffled $EuchreDeck
		for {set i 1} {$i < [llength $Shuffled]} {incr i} {
			set j [expr {int(rand() * [llength $Shuffled])}]
			set Temp [lindex $Shuffled $j]
			set Shuffled [lreplace $Shuffled $j $j [lindex $Shuffled $i]]
			set Shuffled [lreplace $Shuffled $i $i $Temp]
		}
		return $Shuffled
	}
	
	# This proc gets called when a team scores 10 points. This determines whether humans or bots won
	# or lost. We keep track of human wins vs. bot wins.
	proc win_euchre_game {EuchreLobby} {
		variable _euchre_scores_path
		
		set GameScore [$EuchreLobby var_get game_score]
		set Team1Score [lindex [split $GameScore "_"] 0]
		set Team2Score [lindex [split $GameScore "_"] 1]
		if {$Team1Score > $Team2Score} {
			set Winner 1
		} else {
			set Winner 2
		}
		
		foreach Seat {1 2} {
			set FileHandle [open $_euchre_scores_path r]
			array set EuchreScores [read $FileHandle]
			close $FileHandle
			
			set Partner [expr {int(fmod($Seat+1, 4)) + 1}]
			foreach Player [concat [$EuchreLobby var_get players_list] [$EuchreLobby var_get bots_list]] {
				set PlayerType "bot"
				if {[itcl::find objects $Player] eq ""} {
					set PlayerType "human"
					set Player "connection_$Player"
				}
				switch [$Player var_get seat_number] [subst {
					$Seat {
						set ThisPlayerType $PlayerType
					}
					$Partner {
						set ThisPartnerType $PlayerType
					}
				}]
			}
			
			switch $ThisPlayerType {
				"bot" {
					switch $ThisPartnerType {
						"bot" {
							set GameType "bot_with_bot"
						}
						"human" {
							set GameType "bot_with_human"
						}
					}
				}
				"human" {
					switch $ThisPartnerType {
						"bot" {
							set GameType "bot_with_human"
						}
						"human" {
							set GameType "human_with_human"
						}
					}
				}
			}
			incr EuchreScores(${GameType}_games)
			
			if {$Seat == 1} {
				incr EuchreScores(${GameType}_points_for) $Team1Score
				incr EuchreScores(${GameType}_points_against) $Team2Score
			} else {
				incr EuchreScores(${GameType}_points_for) $Team2Score
				incr EuchreScores(${GameType}_points_against) $Team1Score
			}
			if {$Seat == $Winner} {
				incr EuchreScores(${GameType}_wins)
			}
			set FileHandle [open $_euchre_scores_path w]
			puts $FileHandle [array get EuchreScores]
			close $FileHandle
		}
	}
	
	#Next two procs are used to determine which player won the trick.
	proc determine_trick_winner {EuchreLobby} {
		set LeadSuit [$EuchreLobby var_get lead_suit]
		array set CardsArray [$EuchreLobby array_get played_cards_array]
		set Trump [$EuchreLobby var_get trump]
		
		set WinningCard ""
		set WinningSeatNumber ""
		set Cards ""
		foreach {SeatNumber Card} [array get CardsArray] {
			lappend Cards $Card
			if {$WinningCard eq ""} {
				set WinningCard $Card
				set WinningSeatNumber $SeatNumber
				continue
			}
			# puts "about to compare $Card $WinningCard"
			if {$Card eq [compare_two_cards $Card $WinningCard $Trump $LeadSuit]} {
				set WinningCard $Card
				set WinningSeatNumber $SeatNumber
			}
		}
		# puts "WinningCard=$WinningCard, Cards=$Cards"
		return $WinningSeatNumber
	}
	proc compare_two_cards {Card1 Card2 Trump {LeadSuit ""}} {
		set LeftSuit ""
		switch $Trump {
			"H" {set LeftSuit "D"}
			"D" {set LeftSuit "H"}
			"S" {set LeftSuit "C"}
			"C" {set LeftSuit "S"}
		}
		array set NotTrumpCompare {
			9	1
			10	2
			J 	3
			Q	4
			K	5
			A	6
		}
		array set TrumpCompare [subst {
			"9_$Trump"		1
			"10_$Trump"		2
			"J_$LeftSuit"	6
			"J_$Trump"		7
			"Q_$Trump"		3
			"K_$Trump"		4
			"A_$Trump"		5
		}]
		
		set Card1Suit [lindex [split $Card1 "_"] 1]
		set Card2Suit [lindex [split $Card2 "_"] 1]
		if {$Card1Suit eq $Trump || $Card1 eq "J_$LeftSuit"} {
			if {$Card2Suit ne $Trump && $Card2 ne "J_$LeftSuit"} {
				return $Card1
			}
			if {$TrumpCompare($Card1) > $TrumpCompare($Card2)} {
				return $Card1
			} else {
				return $Card2
			}
		}
		if {$Card2Suit eq $Trump || $Card2 eq "J_$LeftSuit"} {
			return $Card2
		}
		
		if {$Card1Suit eq $LeadSuit} {
			if {$Card2Suit ne $LeadSuit} {
				return $Card1
			}
		} else {
			if {$Card2Suit eq $LeadSuit} {
				return $Card2
			}
		}
		set Face1 [lindex [split $Card1 "_"] 0]
		set Face2 [lindex [split $Card2 "_"] 0]
		if {$NotTrumpCompare($Face1) > $NotTrumpCompare($Face2)} {
			return $Card1
		} else {
			return $Card2
		}
	}

# ----------------------------------------JSON Kludge--------------------------------------------		
	
	# This was written before I realized the json::write package. 
	#
	# These are all helper procs for constructing JSON strings
	
	proc get_JSON_players {Lobby} {
		set Players [concat [$Lobby var_get players_list] [$Lobby var_get bots_list]]
		set Result "\["
		foreach Player $Players {
			set PlayerObject "connection_$Player"
			if {[itcl::find objects $PlayerObject -class http_connection] eq ""} {
				set PlayerObject $Player
			}
			if {[$PlayerObject var_get seat_number] eq ""} {
				continue
			}
			append Result [subst {\{"name":"[$PlayerObject var_get nickname]","seat":"[$PlayerObject var_get seat_number]"\},}]
		}
		if {$Result eq "\["} {return "\[\]"}
		set Result [string replace $Result end end "\]"]
		return $Result
	}
	
	proc get_JSON_lobbies {} {
		set Result "\["
		foreach EuchreLobby [itcl::find objects "euchre_*" -class euchre_lobby] {
			set Name [lindex [split "$EuchreLobby" "_"] 1]
			set Name "[string map [list %= " "] $Name]"
			append Result [subst {\{"name":"$Name","players":[get_JSON_players $EuchreLobby]\},}]
		}
		if {$Result eq "\["} {return "\[\]"}
		set Result [string replace $Result end end "\]"]
		return $Result
	}
	
	proc get_JSON_cards {Cards} {
		set Result "\["
		foreach Card $Cards {
			set Face [lindex [split $Card "_"] 0]
			set Suit [lindex [split $Card "_"] 1]
			append Result [subst {\{"id":"$Card","face":"$Face","suit":"$Suit"\},}]
		}
		set Result [string replace $Result end end "\]"]
		if {[llength $Cards] == 1} {
			set Result [string map [list "\[" "" "\]" ""] $Result]
		}
		return $Result
	}
	proc write_JSON {NameValueList} {
		set JSON "\{"
		foreach NameValue [split $NameValueList "\n"] {
			if {$NameValue eq ""} {continue}
			set NameValue [string trim [string trim $NameValue "\t"] " "]
			
			set Braces 0
			if {[string index $NameValue end] eq "\}"} {
				set Braces 1
			}
			foreach {Name Value} $NameValue {
				if {[string index $Value 0] eq "\["} {
					append JSON "\"$Name\":$Value,"
					continue
				}
				if {$Braces} {
					append JSON "\"$Name\":\{$Value\},"
					continue
				}
				append JSON "\"$Name\":\"$Value\","
			}
		}
		set JSON [string replace $JSON end end "\}"]
		set JSON [string map [list "%_bsn" "\\n"] $JSON]
		# puts "JSON=$JSON"
		return $JSON
	}
}

# ----------------------------------------Classes--------------------------------------------		

# Euchre bots are used to fill the empty chairs in a euchre game which has four players.
#
# These bots are smarter than the average drunken euchre player.
itcl::class euchre_bot {
	protected variable euchre_lobby ""
	protected variable euchre_hand ""
	protected variable seat_number ""
	protected variable nickname ""
	protected variable hand_score_counter 0
	
	constructor {} {
		
	}
	destructor {}
	
	# All the logic for what card to play next...
	method play_card {} {
		array set PlayedCards [web_server::$euchre_lobby array_get played_cards_array]
		set Trump [web_server::$euchre_lobby var_get trump]
		set Alone [web_server::$euchre_lobby var_get alone]
		set TrumpCaller [web_server::$euchre_lobby var_get trump_caller]
		
		#First determine if anyone is alone
		set IsAlone 0
		switch $seat_number {
			1 -
			3 {
				if {[lindex [split $Alone "_"] 0] == 1} {
					set IsAlone 1
				}
			}
			2 -
			4 {
				if {[lindex [split $Alone "_"] 1] == 1} {
					set IsAlone 1
				}
			}
		}
		
		# Next, we need to know who lead and what they played
		set Lead $seat_number
		set LeadSuit ""
		set LeadFace ""
		if {[array names PlayedCards] ne ""} {
			while {1} {
				set Lead [expr {int(fmod($Lead, 4)) + 1}]
				if {[array names PlayedCards $Lead] ne ""} {
					break
				}
			}
			set LeadSuit [lindex [split $PlayedCards($Lead) "_"] 1]
			set LeadFace [lindex [split $PlayedCards($Lead) "_"] 0]
		}
		
		# Determine left suit
		switch $Trump {
			"H" {
				if {$LeadSuit eq "D" && $LeadFace eq "J"} {
					set LeadSuit "H"
				}
				set LeftSuit "D"
			}
			"D" {
				if {$LeadSuit eq "H" && $LeadFace eq "J"} {
					set LeadSuit "D"
				}
				set LeftSuit "H"
			}
			"C" {
				if {$LeadSuit eq "S" && $LeadFace eq "J"} {
					set LeadSuit "C"
				}
				set LeftSuit "S"
			}
			"S" {
				if {$LeadSuit eq "C" && $LeadFace eq "J"} {
					set LeadSuit "S"
				}
				set LeftSuit "C"
			}
		}
		
		# Sort cards into Trump, lead suit, and off suit
		set SuitedCards ""
		set TrumpCards ""
		set OffSuitCards ""
		foreach Card $euchre_hand {
			set ThisSuit [lindex [split $Card "_"] 1]
			if {[lindex [split $Card "_"] 0] eq "J" && $LeftSuit eq $ThisSuit} {
				lappend TrumpCards $Card
				if {$LeadSuit eq $Trump} {
					lappend SuitedCards $Card
				}
				continue
			}
			if {$LeadSuit eq $ThisSuit} {
				lappend SuitedCards $Card
				if {$Trump eq $ThisSuit} {
					lappend TrumpCards $Card
				}
				continue
			}
			if {$Trump eq $ThisSuit} {
				lappend TrumpCards $Card
				continue
			}
			lappend OffSuitCards $Card
		}
		
		# Construct a list of trump cards that have not been played
		array set RemainingCards [web_server::$euchre_lobby array_get remaining_cards_array]
		set RemainingTrumpCards $RemainingCards($Trump)
		if {[lsearch $RemainingCards($LeftSuit) "J_$LeftSuit"] >= 0} {
			lappend RemainingTrumpCards "J_$LeftSuit"
		}
		
		# Determine partner and who is currently winning the trick
		set PlayAction ""
		set Partner [expr {int(fmod($seat_number+1, 4)) + 1}]
		set WinningCard [web_server::determine_trick_winner $euchre_lobby]
		if {$WinningCard ne ""} {
			set WinningCard $PlayedCards($WinningCard)
		}
		
		# If we are leading
		if {$Lead == $seat_number} {
			switch $TrumpCaller [subst {
				$Partner  {
					# We are leading and partner made trump.
					if {[lsearch $TrumpCards "J_*"] != -1} {
						# If we have a jack, play it!
						set PlayAction "trump_high_partner"
					} else {
						if {[llength $OffSuitCards] != 0} {
							# Play high off suit if we have it
							set PlayAction "offsuit_high"
						} else {
							# Play high trump if we have no off suit
							set PlayAction "trump_high"
						}
					}
				}
				$seat_number {
					# We are leading and made trump. Play high trump if we have it, otherwise high off suit
					if {[llength $TrumpCards] != 0} {
						set PlayAction "trump_high"
					} else {
						set PlayAction "offsuit_high"
					}
				}
				default {
					# We are leading and other team made trump. High off suit, otherwise high trump
					if {[llength $OffSuitCards] != 0} {
						set PlayAction "offsuit_high"
					} else {
						set PlayAction "trump_high"
					}
				}
			}]
		} else {
			# We are not leading
			if {[llength $SuitedCards] != 0} {
				# following suit
				if {$LeadSuit eq $Trump} {
					set PlayAction "trump"
				} else {
					set PlayAction "suited"
				}
			} else {
				# cannot follow suit, try to trump, otherwise throw away
				if {[llength $TrumpCards] != 0} {
					set PlayAction "trump"
				} else {
					set PlayAction "throw"
				}
			}
		}
		
		# The first four actions are simple. When it comes to playing trump though, it get more complicated
		# When playing trump we look at many different factors to ensure we don't waste trump cards.
		set PlayCard ""
		switch $PlayAction {
			"trump_high_partner" {
				set PlayCard [get_best_card $TrumpCards]
			}
			"offsuit_high" {
				set PlayCard [get_best_card $OffSuitCards]
			}
			"throw" {
				set PlayCard [get_worst_card $OffSuitCards]
			}
			"suited" {
				set PlayCard [get_best_card $SuitedCards]
			}
			"trump_high" {
				# This action only occurs when we are leading
				
				# Start with best trump card
				set PlayCard [get_best_card $TrumpCards]
				
				# If we still have off suit cards and we are not alone; we only play our high trump if
				# it is the highest remaining trump card. Otherwise we play our best off suit.
				if {[llength $OffSuitCards] > 0 && !$IsAlone} {
					foreach RemainingTrumpCard $RemainingTrumpCards {
						if {$RemainingTrumpCard eq $PlayCard} {
							continue
						}
						if {[web_server::compare_two_cards $RemainingTrumpCard $PlayCard $Trump] eq $RemainingTrumpCard} {
							set PlayCard [get_best_card $OffSuitCards]
							break
						}
					}
				}
			}
			"trump" {
				# First, find our smallest trump card that can win the current trick.
				set MyTrumpCards $TrumpCards
				set PlayCard [get_best_card $MyTrumpCards]
				if {[web_server::compare_two_cards $WinningCard $PlayCard $Trump] eq $PlayCard} {
					while {1} {
						set BestTrumpIndex [lsearch $MyTrumpCards $PlayCard]
						set MyTrumpCards [lreplace $MyTrumpCards $BestTrumpIndex $BestTrumpIndex]
						if {[llength $MyTrumpCards] == 0} {
							break
						}
						set NextBest [get_best_card $MyTrumpCards]
						if {[web_server::compare_two_cards $NextBest $WinningCard $Trump] eq $NextBest} {
							set PlayCard $NextBest
						} else {
							break
						}
					}
				} else {
					set PlayCard ""
				}
				
				# If my partner has played
				if {[array names PlayedCards $Partner] ne "" } {
					# And they've played the winning card so far
					if {$PlayedCards($Partner) eq "$WinningCard"} {
						# And they've played an ace, or I am the last to play, throw away
						if {[string first "A" $PlayedCards($Partner)] == 0 || \
							[llength [array names PlayedCards]] == 3 \
						} {
							set PlayCard ""
						}
						
						# Or trump was lead and my partner played a jack of trump, throw away
						if {$LeadSuit eq $Trump && [string first "J" $PlayedCards($Partner)] == 0} {
							set PlayCard ""
						}
					}
				}
				
				# play worst possible card
				if {$PlayCard eq ""} {
					if {$LeadSuit eq $Trump} {
						set PlayCard [get_worst_card $TrumpCards]
					} else {
						set PlayCard [get_worst_card $OffSuitCards]
					}
				}
			}
		}
		
		# If we are still undecided, it's probably a good idea to throw off.
		if {$PlayCard ne ""} {
			return $PlayCard
		} else {
			return [get_worst_card $euchre_hand]
		}
	}
	
	# Used to determine what card to play. Uses compare_two_cards.
	method get_best_card {Cards} {
		set Trump [web_server::$euchre_lobby var_get trump]
		set BestCard ""
		foreach Card $Cards {
			if {$BestCard eq ""} {
				set BestCard $Card
				continue
			}
			set BetterCard [web_server::compare_two_cards $Card $BestCard $Trump]
			if {$BetterCard eq $Card} {
				set BestCard $Card
			}
		}
		return $BestCard
	}
	
	# Used to determine what card to play. Uses compare_two_cards.
	method get_worst_card {Cards} {
		set Trump [web_server::$euchre_lobby var_get trump]
		set WorstCard ""
		foreach Card $Cards {
			if {$WorstCard eq ""} {
				set WorstCard $Card
				continue
			}
			set BetterCard [web_server::compare_two_cards $Card $WorstCard $Trump]
			if {$BetterCard eq $WorstCard} {
				set WorstCard $Card
			}
		}
		return $WorstCard
	}
	
	# This looks funny. Should probably fix it.
	method call_trump_this_suit {OfferedSuit} {
		return [rate_hand $euchre_hand $OfferedSuit]
		
		
		
		set LeftSuit ""
		switch $OfferedSuit {
			"H" {set LeftSuit "D"}
			"D" {set LeftSuit "H"}
			"S" {set LeftSuit "C"}
			"C" {set LeftSuit "S"}
		}
		
		set HasJack 0
		set TrumpCount 0
		foreach Card $euchre_hand {
			set Face [lindex [split $Card "_"] 0]
			set Suit [lindex [split $Card "_"] 1]
			
			if {$Face eq "J"} {
				if {$Suit eq $OfferedSuit || $Suit eq $LeftSuit} {
					incr HasJack
					incr TrumpCount
					continue
				}
			}
			if {$Suit eq $OfferedSuit} {
				incr TrumpCount
			}
		}
		if {$TrumpCount > 2 && $HasJack > 0} {
			return 1
		}
		return 0
	}
	
	# Simulates a bunch of hands, rates them, then writes the results to file. This is for testing the 
	# hand rating algorithm.
	method simulate_hand_ranking {} {
		set FileHandle [open [file join $_source_path euchre_hand_scores.txt] w]
		array set ScoresArray ""
		for {set i 0} {$i < 10000} {incr i} {
			set Deck [web_server::shuffle_cards]
			set Hand [lrange $Deck 6 10]
			set BestScore -50
			foreach Suit {H D C S} {
				set Score [rate_hand $Hand $Suit]
				if {$Score > $BestScore} {
					set BestScore $Score
				}
			}
			puts "$BestScore"
			if {[array names ScoresArray $BestScore] eq ""} {
				set ScoresArray($BestScore) 1
			} else {
				incr ScoresArray($BestScore)
			}
		}
		foreach {Score Frequency} [array get ScoresArray] {
			puts $FileHandle "$Score"
		}
		puts $FileHandle "--------------------------------------"
		foreach {Score Frequency} [array get ScoresArray] {
			puts $FileHandle "$Frequency"
		}
		close $FileHandle
	}
	
	# This is a complicated algorithm for rating hands. If a Trump argument is not provided, this method
	# will rate the hand in every suit, returning the score of the best suit.
	#
	# Several different factors contribute to the hand score including, # of trump cards, having large trump
	# cards, off suit aces, etc. We have found that a good threshold for trump is a score > 11.
	method rate_hand {Cards {Trump "C D H S"}} {
		if {[llength $Trump] > 1} {
			set BestScore -50
			foreach Suit $Trump {
				set ThisScore [rate_hand $Cards $Suit]
				if {$ThisScore > $BestScore} {
					set BestScore $ThisScore
				}
			}
			return $BestScore
		}
		
		set LeftSuit ""
		set TrumpCards ""
		set OffSuitCards ""
		switch $Trump {
			"H" {set LeftSuit "D"}
			"D" {set LeftSuit "H"}
			"S" {set LeftSuit "C"}
			"C" {set LeftSuit "S"}
		}
		
		# Sort cards into trump and off suit cards
		foreach Card $Cards {
			set ThisSuit [lindex [split $Card "_"] 1]
			if {[lindex [split $Card "_"] 0] eq "J" && $LeftSuit eq $ThisSuit} {
				lappend TrumpCards $Card
				continue
			}
			if {$Trump eq $ThisSuit} {
				lappend TrumpCards $Card
				continue
			}
			lappend OffSuitCards $Card
		}
		
		set HandScore 0
		# Number of trump cards has huge weight on the hand score.
		switch [llength $TrumpCards] {
			1 {incr HandScore -10}
			2 {incr HandScore -4}
			3 {incr HandScore 2}
			4 {incr HandScore 5}
			5 {incr HandScore 6}
		}
		
		# The larger trump cards are obviously worth more points.
		foreach TrumpCard $TrumpCards {
			switch -glob -- $TrumpCard [subst {
				9_$Trump	{incr HandScore 2}
				10_$Trump	{incr HandScore 2}
				Q_$Trump	{incr HandScore 3}
				K_$Trump	{incr HandScore 4}
				A_$Trump	{incr HandScore 6}
				J_$LeftSuit {incr HandScore 8}
				J_$Trump	{incr HandScore 10}
			}]
		}
		
		# Separate off suit cards into the different suits.
		array unset OffSuitArray
		foreach OffSuit $OffSuitCards {
			lappend OffSuitArray([lindex [split $OffSuit "_"] 1]) $OffSuit
		}
		
		# Having all four suits will lose many points, while only having one suit other than trump is a plus.
		# Off suit aces are rewarded a few points.
		switch [llength [array names OffSuitArray]] {
			0 -
			1 {
				incr HandScore 3
				foreach {Suit Card} [array get OffSuitArray] {
					if {[string first "A" $Card] != -1} {
						incr HandScore 3
					}
				}
			}
			2 {
				incr HandScore -2
				foreach {Suit Card} [array get OffSuitArray] {
					if {[string first "A" $Card] != -1} {
						incr HandScore 4
					}
				}
			}
			3 {
				incr HandScore -7
				foreach {Suit Card} [array get OffSuitArray] {
					if {[string first "A" $Card] != -1} {
						incr HandScore 4
					}
				}
			}
		}
		
		# A further look into off suit cards.
		foreach {Suit Card} [array get OffSuitArray] {
			set Face [lindex [split $Card "_"] 0]
			if {[llength $OffSuitArray($Suit)] == 1} {
				# Low off suit cards will lose the hand points if the king or ace of that same suit
				# is not also in the hand.
				switch $Face {
					9 -
					10 {incr HandScore -2}
					J -
					Q {incr HandScore -1}
				}
			} else {
				# else case means there are 2 or more cards of this off suit.
				
				# minus 2 points for having 3 cards of the same off suit.
				if {[llength $OffSuitArray($Suit)] == 3} {
					incr HandScore -2
				}
				
				# No further deduction if the king/ace of this suit is in the hand
				if {[lsearch $OffSuitArray($Suit) "A*"] >= 0 || \
					[lsearch $OffSuitArray($Suit) "K*"] >= 0 \
				} {
					continue
				}
				
				# Minus 2 if highest card in this suit is Queen
				if {[lsearch $OffSuitArray($Suit) "Q*"] >= 0} {
					incr HandScore -2
					continue
				}
				
				# Minus 4 if highest card in this suit is Jack
				if {[lsearch $OffSuitArray($Suit) "J*"] >= 0} {
					incr HandScore -4
					continue
				}
				
				# Minus 6 if highest card in this suit is 10
				incr HandScore -6
			}
		}
		# puts "rate_hand ($Trump)...$Cards, $HandScore"
		return $HandScore
	}
	
	# The bot has just been ordered up and must discard. This looks at all 6 hand possibilities and discards
	# the worst card.
	method dealer_discard {OrderedCard} {
		set Trump [lindex [split $OrderedCard "_"] 1]
		
		set Discard $OrderedCard
		set BestHandScore [rate_hand $euchre_hand $Trump]
		for {set i 0} {$i < [llength $euchre_hand]} {incr i} {
			set TempHand [lreplace $euchre_hand $i $i $OrderedCard]
			set ThisHandScore [rate_hand $TempHand $Trump]
			if {$ThisHandScore > $BestHandScore} {
				set BestHandScore $ThisHandScore
				set Discard [lindex $euchre_hand $i]
			}
		}
		# puts "echure_bot dealer_discard...hand=$euchre_hand, OrderedCard=$OrderedCard, Discard=$Discard"
		return $Discard
	}
	
	# Same as last method but the bot is deciding whether to pick up the flipped card.
	method dealer_pick_up {OrderedCard} {
		set Trump [lindex [split $OrderedCard "_"] 1]
		
		set Discard $OrderedCard
		set BestHandScore [rate_hand $euchre_hand $Trump]
		for {set i 0} {$i < [llength $euchre_hand]} {incr i} {
			set TempHand [lreplace $euchre_hand $i $i $OrderedCard]
			set ThisHandScore [rate_hand $TempHand $Trump]
			if {$ThisHandScore > $BestHandScore} {
				set BestHandScore $ThisHandScore
				set Discard [lindex $euchre_hand $i]
			}
		}
		return $BestHandScore
	}
	method var_get {VarName} {
		return [set $VarName]
	}
	method var_set {VarName Value} {
		set $VarName $Value
	}
	method get_trash_talk {Name Partner} {
		set TrashTalkList [subst {
			"$Name sucks dick for bus money, then walks"
			"An empty chair would make better plays than $Name"
			"Who invited $Name? Seriously, that guy's a jerk"
			"Go die in a hole, $Name"
			"Fuck off, $Name"
			"${Name}'s mom asked to stick a finger in my butt, but I only let ${Partner}'s mom do that"
			"I'm not even human and still play better cards than $Name"
			"Eat shit, $Name"
		}]
		return [lindex $TrashTalkList [expr {int(rand() * [llength $TrashTalkList])}]]
	}
}

# This class keeps track of all the same data as a bot, plus all the socket and http information.
# There is not logic for how to play since that's up to the user. These objects are strictly for holding data.
itcl::class http_connection {
	protected variable http_header_in
	protected variable http_header_out
	protected variable query
	protected variable upgrade_to_https ""
	protected variable socket ""
	protected variable is_web_socket 0
	
	protected variable euchre_lobby ""
	protected variable seat_number ""
	protected variable nickname ""
	protected variable euchre_hand ""
	
	protected variable hand_score_counter 0
	
	constructor {Socket {Upgrade 0}} {
		set socket $Socket
		set upgrade_to_https $Upgrade
		
		set euchre_lobby ""
		array set http_header_in ""
		
		array set get_header_out {
			"Content-Type" "text/html"
			"Connection" "close"
		}
	}
	destructor {
		close $socket
	}

	method var_get {VarName} {
		return [set $VarName]
	}
	method var_set {VarName Value} {
		set $VarName $Value
	}
	method array_get {ArrayName} {
		return [array get $ArrayName]
	}
	method array_set {ArrayName Array} {
		array set $ArrayName $Array
	}
	method set_http_header_out {NameValueList} {
		foreach {Name Value} $NameValueList {
			set http_header_out($Name) $Value
		}
	}
}

# No logic in here either. Also used for getting and setting data. It might be a good idea to move
# the game flow logic here.
itcl::class euchre_lobby {
	public variable players_list ""
	public variable bots_list ""
	public variable top_card ""
	public variable top_card_suit ""
	public variable dealer 0
	public variable lead_suit ""
	public variable trump ""
	public variable trump_caller ""
	public variable alone "0_0"
	public variable trick_score "0_0"
	public variable game_score "0_0"
	public variable last_command ""
	
	public variable hand_counter 0
	
	public variable played_cards_array
	public variable remaining_cards_array
	
	constructor {{PlayerSocket ""}} {
		if {$PlayerSocket ne ""} {
			set players_list [list $PlayerSocket]
		}
		array unset played_cards_array
	}
	destructor {
		foreach Player $players_list {
			web_server::connection_$Player var_set euchre_lobby ""
		}
		foreach Bot $bots_list {
			itcl::delete object web_server::$Bot
		}
	}
	method re_init {} {
		set trick_score "0_0"
		set game_score "0_0"
	}
	method append_players_list {PlayerSocket} {
		lappend players_list $PlayerSocket
	}
	method append_bots_list {BotHandle} {
		lappend bots_list $BotHandle
	}
	method remove_from_lobby {PlayerSocket} {
		set Index [lsearch $players_list $PlayerSocket] 
		set players_list [lreplace $players_list $Index $Index]
	}
	method var_get {VarName} {
		return [set $VarName]
	}
	method var_set {VarName Value} {
		set $VarName $Value
	}
	method set_played_card {Seat Card} {
		set played_cards_array($Seat) $Card
		
		set Suit [lindex [split $Card "_"] 1]
		if {$Suit eq ""} {return}
		
		set CardIndex [lsearch $remaining_cards_array($Suit) $Card]
		set remaining_cards_array($Suit) [lreplace $remaining_cards_array($Suit) $CardIndex $CardIndex]
	}
	method array_get {ArrayName} {
		return [array get $ArrayName]
	}
	method array_unset {ArrayName} {
		array unset $ArrayName
	}
	method set_up_remaining_cards_array {} {
		array set remaining_cards_array {
			C "9_C 10_C J_C Q_C K_C A_C"
			S "9_S 10_S J_S Q_S K_S A_S"
			H "9_H 10_H J_H Q_H K_H A_H"
			D "9_D 10_D J_D Q_D K_D A_D"
		}
	}
}


web_server::main






