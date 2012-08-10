--------------------------------------------------------------------------------------------
-- INITIALIZE
-- Runs when skin is loaded or refreshed. Sets up the board, declares global variables, and
-- creates a database containing the properties of each square.

function Initialize()

	---------------------------------------------------------------------
	-- GET USER SETTINGS FROM SKIN
	
	iMines     = tonumber(SKIN:GetVariable('NumberOfMines', '10'))
	iRows      = tonumber(SKIN:GetVariable('NumberOfRows',  '9' ))
	iCols      = tonumber(SKIN:GetVariable('NumberOfCols',  '9' ))
	iQuestions = tonumber(SKIN:GetVariable('Questions',     '0' ))
	
	-- These basic variables determine all of the parameters for the
	-- current game. The tonumber() wrapper means we can use their
	-- numerical values in math formulas.
	
	---------------------------------------------------------------------
	-- CREATE SQUARES DATABASE
	
	tSquares = {}
	
	for i = 1, iRows * iCols do
		local iX = (i-1) - math.floor((i-1) / iCols)*iCols
		local iY = math.floor((i-1) / iCols)
		tSquares[i] = { z=i, x=iX, y=iY, m=0, n=0, f=0, q=0, c=0 }
	end
	
	-- Here, we calculate each square's X and Y coordinates as a function
	-- of the square's linear index number. (For example, in a 10x10 grid,
	-- square #25 is at position [4,2] - fifth square on the third row.)
	-- All squares are also initially set as unmined, not adjacent to any
	-- mines, not flagged or question-marked, and not cleared.
	-- Technically, each "square" in the database is a database itself -
	-- a table within a table.
	
	---------------------------------------------------------------------
	-- CREATE SQUARES GRID IN SKIN
	
	for i = 2, #tSquares do
		if (i-1) % iCols == 0 then
			SKIN:Bang('!SetOption '..i..' MeterStyle "StyleSquare | StyleNewRow"')
		else
			SKIN:Bang('!SetOption '..i..' MeterStyle "StyleSquare"')
		end
		SKIN:Bang('!ShowMeter '..i)
	end
	
	-- The skin supports up to 720 squares. Since not all squares are
	-- needed for most games, all but #1 start in a hidden state. This
	-- loop reveals those squares needed for the current configuration,
	-- and creates "line breaks" based on the number of columns. (For
	-- example, if there are 9 squares per row, then every 9th square
	-- gets the "new row" property, which is defined as a MeterStyle in
	-- the skin.)
	
	---------------------------------------------------------------------
	-- DECLARE GLOBAL VARIABLES
	
	iFlags = iMines
	SKIN:Bang('!SetOption Flags Text '..string.format('%03d', iFlags))
	
	-- The player is given a number of "flags" equal to the number of
	-- mines, in order to mark spots where she thinks a mine might be
	-- hidden. This number will increase or decrease as flag markers are
	-- added and removed. The bang displays this number in the "Flags"
	-- meter in the skin. (The "%03d" format ensures a 3-digit layout.)
	
	iStartTime = 0
	iTimer = 0
	SKIN:Bang('!SetOption Timer Text '..string.format('%03d', iTimer))
	
	-- The timer starts at zero, and is displayed in the same format as
	-- the Flags counter. The "start time" variable serves two purposes:
	-- not only does it record the exact moment when the game begins, but
	-- the "timer" will not increase until the start time has been set
	-- (exceeds zero).
	
	iCleared = 0
	iGameOver = 0
	
	-- Declares that no squares have been cleared yet, and the game has
	-- not ended. Other functions will need to know both of these things.
	
	sLevel = DetectLevel(iRows, iCols, iMines)
	Scores(sLevel)
	Settings()
	
	-- The DetectLevel() function determines whether the current settings
	-- match the "Beginner," "Intermediate" or "Advanced" conditions. If
	-- not, the game is "Custom." The Scores() and Settings() functions
	-- update the Scores and Settings menu panels, respectively. (As a
	-- courtesy, the Scores panel starts on the tab for the current
	-- difficulty level.)
	
end

--------------------------------------------------------------------------------------------
-- UPDATE
-- Runs whenever the skin updates: once per second, by default. Updates the timer value (if
-- the start time has already been set).

function Update()
	if iStartTime > 0 then
		iTimer = os.difftime(os.time(), iStartTime)
		iTimer = iTimer < 1000 and iTimer or 999
		SKIN:Bang('!SetOption Timer Text '..string.format('%03d', iTimer))
	end
	
	-- Since the skin updates irregularly (every time the player clicks a square), we cannot
	-- reliably record the amount of time elapsed just by counting updates. Instead, we
	-- record the time when the game started - the player's first click - and repeatedly
	-- compare that time to the Windows clock. We also arbitrarily freeze the clock at 999
	-- seconds, as an homage to the original Minesweeper.
	
end

--------------------------------------------------------------------------------------------
-- CLEARING ACTIONS
-- Run when the player performs an action that clears one or more squares, including left-
-- clicking, double-clicking, or invoking the "Clear All" shortcut from the menu. All three
-- actions feed into a common "Clear()" function, providing a "queue" of squares for the
-- function to process.

function LeftClick(z)
	local z = tonumber(z)
	
	-- In this script, "z" is always a reference to a square's linear
	-- index number. When the player clicks on a square, it calls this
	-- function with its index number as a parameter, telling the script
	-- which square to manipulate. (Each square's meter in the skin is
	-- named by its number, which makes this a lot simpler.)
	
	---------------------------------------------------------------------
	-- SAFETY CHECKS
	
	if tSquares[z]['c'] == 1 or tSquares[z]['f'] == 1 or iGameOver ~= 0 then
		return
	end
	
	-- Left-clicking does nothing if the square has already been cleared,
	-- if it has a flag marker, or if the game has ended.
	
	---------------------------------------------------------------------
	-- FIRST-CLICK ACTIONS: START TIMER, PLANT MINES
	
	if iStartTime == 0 then
		iStartTime = os.time()
	
		-- Starts the timer by recording the current time. This only happens
		-- on the first left-click of each game.
		
		local iUnplantedMines = iMines
		while iUnplantedMines > 0 do
			local iRandomSquare = math.random(1,#tSquares)
			if tSquares[iRandomSquare]['m'] == 0 and tSquares[iRandomSquare]['c'] == 0 and iRandomSquare ~= z then
				tSquares[iRandomSquare]['m'] = 1
				iUnplantedMines = iUnplantedMines -1
			end
		end
		
		-- Pick random squares to become mines. This code loops through the
		-- entire table, each time picking a random uncleared square and
		-- "planting" a mine in that space (if one hasn't already been
		-- planted there). The loop breaks when the correct number of mines
		-- have been deployed. We do this only after the first square has
		-- been clicked in order to make sure that the player never loses
		-- on her first move.
		
		for i = 1, #tSquares do
			tSquares[i]['n'] = Adjacents(i, 'Threats')
		end
		
		-- Once all mines have been planted, the Adjacents() function checks
		-- each square and calculates the number of mines adjacent to that
		-- square. Adjacents() is split off into a separate function because
		-- its features are used in several places.
	end
	
	---------------------------------------------------------------------
	-- CREATE QUEUE AND CLEAR
	
	tZ = { z }
	Clear(tZ, 1)
	
	-- The "Clear()" function requires a list of squares in the form of a
	-- table. Since left-clicking only affects one square, we create a
	-- table with one cell, containing just the ID of the current square.
	-- The Clear() function will take over from here.
	
end

function LeftDoubleClick(z)
	local z = tonumber(z)
	
	---------------------------------------------------------------------
	-- SAFETY CHECKS

	if tSquares[z]['c'] == 0 or tSquares[z]['n'] == 0 or tSquares[z]['f'] == 1 or iGameOver ~= 0 then
		return
	end
	
	-- The game allows the player to double-click an empty square in
	-- order to clear all unflagged squares adjacent to it. Unlike a
	-- normal click, this can only be done on a square that has already
	-- been cleared. It also ignores any squares with no adjacent mines,
	-- since those squares will already have been cleared all around.
	-- Otherwise, it makes the same checks as a normal click.
	
	---------------------------------------------------------------------
	-- GET ADJACENT SQUARES

	local tAdjacents = Adjacents(z)
	
	-- Without being given any other parameters, the Adjacents() function
	-- returns the complete list of a square's immediate neighbors.
	
	---------------------------------------------------------------------
	-- CHECK FOR SUFFICIENT FLAGS
	
	local iAdjacentFlags = 0
	for i,v in ipairs(tAdjacents) do
		if tSquares[v]['f'] == 1 then
			iAdjacentFlags = iAdjacentFlags + 1
		end
	end
	if iAdjacentFlags < tSquares[z]['n'] then
		Message('TooFewFlags')
		return
	end
	
	-- In order to protect the player from their own stupidity, this loop
	-- counts up the number of flags among the adjacent squares, and
	-- terminates if there isn't at least one flag for every mine. Of 
	-- course, it's still possible that the flags have been wrongly
	-- placed, but when there are fewer flags than mines, the player is
	-- guaranteed to lose if the action continues.
	
	---------------------------------------------------------------------
	-- CREATE QUEUE AND CLEAR
	
	Clear(tAdjacents, 1)
	
	-- This is the same function that ends the normal LeftClick() action.
	-- But this time, we are sending the entire table of adjacent squares
	-- to process as a batch. The effect is simply as if the player had
	-- clicked all of these squares simultaneously.
	
end

function ClearAll()
	
	---------------------------------------------------------------------
	-- SAFETY CHECKS
	
	if iGameOver ~= 0 then
		return
	end
	
	-- The third and final clearing action is "Clear All," which the
	-- player can select from the menu to clear the board of all
	-- unflagged squares. Because this action isn't associated with a
	-- specific square, all we do as an initial safety check is to
	-- terminate if the game has already anded.
	
	if iStartTime == 0 then
		Message('Suicide')
		return
	end
	
	-- If the game hasn't started yet, then all squares must still be
	-- unclicked - which means that the player is trying to clear the
	-- board without having any information about the mine placement.
	-- In this case, we terminate the action with a feedback message
	-- explaining to him what a stupid idea this was.
	
	if iFlags > 0 then
		Message('TooFewFlagsAll')
		return
	end
	
	-- Next, we make the same check as the double-click action: if the
	-- player has placed fewer flags than the total number of mines, we
	-- terminate the action in order to prevent a guaranteed loss.
	
	---------------------------------------------------------------------
	-- CREATE QUEUE AND CLEAR
	
	local tAll = {}
	for i = 1, #tSquares do table.insert(tAll, i) end
	Clear(tAll)
	
	-- Finally, we send the queue - which, in this case, consists of
	-- literally the entire table of squares - to the Clear() function.
	-- And now we'll find out exactly what happens there.
	
end

function Clear(tQueue, iAdjacents)

	---------------------------------------------------------------------
	-- MARK SQUARES CLEAR, CHECK FOR MINES
	
	local iTrippedMines = 0
	for i,v in ipairs(tQueue) do
	
	-- First, the function loops through the list of squares it was given,
	-- performing checks on each individual square in turn.
	
		if tSquares[v]['c'] == 0 and tSquares[v]['f'] == 0 then
			
		-- Squares which are flagged, or have already been cleared, are
		-- ignored.
			
			tSquares[v]['c'] = 1
			iCleared = iCleared + 1
			
			-- The square is marked as cleared in the database. In addition, we
			-- update the variable "iCleared," which records the total number of
			-- cleared squares. This isn't strictly necessary - we can run
			-- through the database and count squares with a certain property at
			-- any time - but as long as we're here, we can save ourselves a
			-- little time this way.
			
			if tSquares[v]['m'] == 1 then
				iTrippedMines = iTrippedMines + 1
				Render('TrippedMine', v)

				-- If the square turns out to be a mine, we mark it as "tripped." The
				-- Render() function handles changing the square's actual appearance
				-- in the skin, changing the square's text, colors, tooltips, etc.
				
			else
				Render('Clear', v)
				if iAdjacents then Adjacents(v, 'Clear') end
				
				-- Otherwise, the square is "clear," in which case we send it off to
				-- the Adjacents() function for some extra checks. Specifically, if
				-- the square is both empty and has no adjacent mines, we do the
				-- player the courtesy of clearing all the neighboring squares -
				-- and then doing the same check on *those* squares, until the entire
				-- formation of contiguous unthreatened squares has been emptied.
				-- The "iCleared" counter will also be increased to account for each
				-- of the affected squares.
				
			end
		end
	end
	
	---------------------------------------------------------------------
	-- CHECK ENDGAME CONDITIONS
	
	if iTrippedMines > 0 then
	
		---------------------------------------------------------------------
		-- LOSS
	
		iGameOver = -1
		Update()
		iStartTime = 0
		Scores(sLevel, 'Update')
		
		-- At this point, if the player has tripped any mines, then we know
		-- she has lost the game. We change the "iGameOver" variable to
		-- indicate that the game has ended in a loss (-1), freeze the timer
		-- after one last update, and send a command to the Scores() function
		-- to update the player's statistics for the current difficulty level.
		
		for i = 1, #tSquares do
			if tSquares[i]['c'] == 0 then
				if tSquares[i]['m'] == 1 and tSquares[i]['f'] == 0 then
					Render('UntrippedMine', i)
				elseif tSquares[i]['m'] == 0 and tSquares[i]['f'] == 1 then
					Render('WrongFlag', i)
				elseif tSquares[i]['m'] == 1 and tSquares[i]['f'] == 1 then
					Render('RightFlag', i)
				else
					Render('Remainder', i)
				end
			end
		end
		
		-- The previous loop rendered all the newly-cleared squares; now we
		-- have to deal with the ones that were left uncleared when the game
		-- ended, revealing the locations of mines that were correctly
		-- flagged, mines that were never detected, and flags that were
		-- wrongly placed.
		
		Message('Lose')
		SKIN:Bang('!SetOption Background SolidColor "#ColorBackgroundLose#"')
		SKIN:Bang('#OpenMenu#')
		
		-- Finally, we send a feedback message informing the player of her
		-- loss, along with a red coloration on the background. We also open
		-- the menu, since she'll probably want to either start a new game or
		-- close the skin in disgust, depending on her mood.
		
	elseif iCleared == #tSquares - iMines then
	
		---------------------------------------------------------------------
		-- WIN
	
		iGameOver = 1
		Update()
		iStartTime = 0
		Scores(sLevel, 'Update')
		
		-- If the player has left a number of uncleared squares equal to the
		-- number of mines - and has not tripped any mines up to this point -
		-- then we know that she has won the game. We don't need to make her
		-- flag all of the mines, since the numbers leave no doubt about
		-- their locations.
		
		for i = 1, #tSquares do
			if tSquares[i]['c'] == 0 then 
				if tSquares[i]['f'] == 0 then
					tSquares[i]['f'] = 1
					iFlags = iFlags - 1
				end
				Render('DefusedMine', i)
			end
		end
		SKIN:Bang('!SetOption Flags Text '..string.format('%03d', iFlags))
		
		-- We don't need to do nearly as much rendering this time, since the
		-- only remaining uncleared squares are necessarily mines. The last
		-- loop automatically flags these squares and renders them as
		-- "defused" mines, indicating victory.
		
		Message('Win')
		SKIN:Bang('#OpenMenu#')
		SKIN:Bang('!SetOption Background SolidColor "#ColorBackgroundWin#"')
		
		-- We end with the opposite equivalents of the "defeat" feedback
		-- messages.
		
	else
		Message('Close')
		SKIN:Bang('#CloseMenu#')
		
		-- If the player has neither won nor lost yet, we simply end the
		-- action by dismissing any feedback messages from the last cycle,
		-- and close the menu (in case the "Clear All" command was selected.)
		
	end
end

--------------------------------------------------------------------------------------------
-- OTHER ACTIONS
-- Other independent gameplay actions, including right-clicking to "flag" a square, as well
-- as restarting the current scenario with a new minefield.

function RightClick(z)
	local z = tonumber(z)
	
	-- Right-clicking allows the player to "flag" a square that she knows
	-- (or thinks) is a mine. Flagged squares are immune to clearing
	-- actions.
	
	---------------------------------------------------------------------
	-- SAFETY CHECKS
	
	if tSquares[z]['c'] == 1 or iGameOver ~= 0 then
		return
	end
	
	-- The action will not proceed if the square is already cleared, or
	-- if the game has ended.
	
	---------------------------------------------------------------------
	-- CYCLE FLAG STATES
	
	if tSquares[z]['f'] == 0 then
		tSquares[z]['f'] = 1
		iFlags = iFlags - 1
		Render('Flag', z)
		
		-- The first condition changes a blank square to a flagged square.
		-- We update the square's properties in the database, reduce the
		-- number of available flags by one, and change the square's
		-- color in the skin to mark its special status.
		
	elseif tSquares[z]['q'] == 0 and iQuestions == 1 then
		tSquares[z]['q'] = 1
		Render('Question', z)
		
		-- If the player has enabled the "question marks" setting, a
		-- second right-click changes a normal flag to a "question" flag.
		-- This is purely for the player's benefit; mechanically, both
		-- types of flags behave exactly the same way.
		
	else
		tSquares[z]['f'] = 0
		tSquares[z]['q'] = 0
		iFlags = iFlags + 1
		Render('Opaque', z)
		
		-- If question marks are not enabled, or the square is already a
		-- question mark, a final right-click changes the square back to
		-- normal and re-adds to the flag counter.
		
	end
	SKIN:Bang('!SetOption Flags Text '..string.format('%03d', iFlags))
	Message('Close')
	
	-- All flagging or unflagging updates the flag counter display in the
	-- skin, and dismisses feedback messages left over from the previous
	-- action.
	
end

function NewGame()
	for i = 1, #tSquares do
		Render('Opaque', i)
	end
	Message('Close')
	SKIN:Bang('!SetOption Background SolidColor "#ColorBackground#"')
	SKIN:Bang('#CloseMenu#')
	Initialize()
	
	-- The "new game" function resets the skin to the state it was in
	-- when the skin was first loaded. All squares are rendered as opaque,
	-- feedback messages are dismissed, the menu is closed, and the
	-- background color loses its win/loss highlighting. Once that's done,
	-- the entire initialization function is run again, resetting global
	-- variables and rebuilding the database from scratch.
	
end

----------------------------------------------------------------------------------------------
-- HELPER FUNCTIONS
-- These scripts have been split into separate functions because they're used in more than one
-- place; their parameters change depending on what kind of information they're given by their
-- "parent" functions. Defining them in this way means that we only have to write the same
-- code one time, instead of pasting multiple copies in different places.

function DetectLevel(r, c, m)
	if     r == 9  and c == 9  and m == 10 then
		return 'Beginner'
	elseif r == 16 and c == 16 and m == 40 then
		return 'Intermediate'
	elseif r == 16 and c == 30 and m == 99 then
		return 'Advanced'
	else
		return 'Custom'
	end
	
	-- This function takes a number of rows, columns and mines, and
	-- determines whether they correspond to one of the standardized
	-- difficulty levels. This is used in two places: in Initialize(),
	-- to get the level of the current game, and in Settings(), to get
	-- the level of the new settings chosen by the player.
	
end

function CoordsToSquare(x,y)
	local z = x + y*iCols + 1
	return z
	
	-- This function takes the X and Y coordinates of a square and
	-- calculates the square's linear index number (Z) - the reverse of
	-- the formula in Initialize() that gets X and Y from Z. This is used
	-- several times in Adjacents() to identify one square based on its
	-- spacial relationship to another.
	
end

function Adjacents(z, sRequest)

	---------------------------------------------------------------------
	-- CREATE TABLE OF ADJACENT SQUARES

	local tAdjacents = {}
	
	-- This function's most basic purpose is to determine the immediate
	-- neighbors of a given square. As usual when dealing with a set of
	-- multiple data points, we create a table to contain the results.
	
	local x = tSquares[z]['x']
	local y = tSquares[z]['y']
	if x > 0         and y > 0         then table.insert(tAdjacents, CoordsToSquare(x - 1, y - 1)) end
	if                   y > 0         then table.insert(tAdjacents, CoordsToSquare(x    , y - 1)) end
	if x < (iCols-1) and y > 0         then table.insert(tAdjacents, CoordsToSquare(x + 1, y - 1)) end
	if x < (iCols-1)                   then table.insert(tAdjacents, CoordsToSquare(x + 1, y    )) end
	if x < (iCols-1) and y < (iRows-1) then table.insert(tAdjacents, CoordsToSquare(x + 1, y + 1)) end
	if                   y < (iRows-1) then table.insert(tAdjacents, CoordsToSquare(x    , y + 1)) end
	if x > 0         and y < (iRows-1) then table.insert(tAdjacents, CoordsToSquare(x - 1, y + 1)) end
	if x > 0                           then table.insert(tAdjacents, CoordsToSquare(x - 1, y    )) end
	
	-- This is probably the most complicated section of the entire script.
	-- Every square may have as many as 8 neighbors, and we check whether
	-- each one exists based on the square's coordinates. For example, if
	-- a square's X=5, we know that another square must exist to its
	-- immediate left at X=4. This is also a necessary, but not
	-- sufficient, condition for the squares to the upper-left and lower-
	-- left, which also depend on the square's Y. Here's a diagram of all
	-- of a square's possible neighbors:
	
	-- [X-1, Y-1]   [X, Y-1]     [X+1, Y-1]
	-- Upper-Left   Upper        Upper-Right
	--
	-- [X-1, Y]     [X, Y]       [X+1, Y]
	-- Left                      Right
	--
	-- [X-1, Y+1]   [X, Y+1]     [X+1, Y+1]
	-- Lower-Left   Lower        Lower-Right
	
	-- For the right and bottom edges, we subtract 1 from the number of
	-- columns and rows because the coordinates start at 0, not 1. For
	-- example, in a game with 16 columns, a square on the right edge
	-- would have X=15. This might seem inconvenient, but it actually
	-- makes the math easier in other places.
	
	-- Once we know that a certain neighboring square exists, we enter
	-- its coordinates into the "Adjacents" table, using the earlier
	-- "CoordsToSquare" function to convert its known X-Y coordinates
	-- into its Z index number.
	
	---------------------------------------------------------------------
	-- COUNT THREATS
	
	local iThreats = 0
	for i,v in ipairs(tAdjacents) do
		if tSquares[v]['m'] == 1 then
			iThreats = iThreats + 1
		end
	end
	
	-- Now that we have a complete list of this square's neighbors, we
	-- can quickly loop through them and count how many of them are mines.
	
	---------------------------------------------------------------------
	-- PERFORM REQUESTED ACTIONS
	
	if sRequest == 'Clear' and iThreats == 0 then
		for i,v in ipairs(tAdjacents) do
			if tSquares[v]['m'] == 0 and tSquares[v]['f'] == 0 and tSquares[v]['c'] == 0 then
				iCleared = iCleared + 1
				tSquares[v]['c'] = 1
				Render('Clear', v)
				Adjacents(v, 'Clear')
			end
		end
		
		-- This action is requested by the Clear() function. When a
		-- square is successfully cleared, and has no adjacent mines, we
		-- also clear the surrounding squares as a courtesy, to save
		-- time. This function is called recursively until no more
		-- unthreatened squares can be found.
		
	elseif sRequest == 'Threats' then
		return iThreats
	else
		return tAdjacents
	end
end

--------------------------------------------------------------------------------------------

function Render(sRequest, z)
	if sRequest == 'Opaque' then
		SKIN:Bang('!SetOption '..z..' SolidColor "#ColorSquare#"')
		SKIN:Bang('!SetOption '..z..' Text ""')
		SKIN:Bang('!SetOption '..z..' ToolTipTitle ""')
		SKIN:Bang('!SetOption '..z..' ToolTipText ""')
		SKIN:Bang('!SetOption '..z..' MouseOverAction """[!SetOption #*CURRENTSECTION*# SolidColor "#ColorSquareRevealed#"][!Update]"""')
		SKIN:Bang('!SetOption '..z..' MouseLeaveAction """[!SetOption #*CURRENTSECTION*# SolidColor "#ColorSquare#"][!Update]"""')
	elseif sRequest == 'Clear' then
		SKIN:Bang('!SetOption '..z..' SolidColor "#ColorSquareClear#"')
		if tSquares[z]['n'] > 0 then
			SKIN:Bang('!SetOption '..z..' Text "'..tSquares[z]['n']..'"')
		end
		SKIN:Bang('!SetOption '..z..' MouseOverAction "[]"')
		SKIN:Bang('!SetOption '..z..' MouseLeaveAction "[]"')
	elseif sRequest == 'Flag' then
		SKIN:Bang('!SetOption '..z..' SolidColor "#ColorSquareFlag#"')
		SKIN:Bang('!SetOption '..z..' MouseOverAction "[]"')
		SKIN:Bang('!SetOption '..z..' MouseLeaveAction "[]"')
	elseif sRequest == 'Question' then
		SKIN:Bang('!SetOption '..z..' SolidColor "#ColorSquareQuestion#"')
		SKIN:Bang('!SetOption '..z..' Text "?"')
	elseif sRequest == 'DefusedMine' then
		SKIN:Bang('!SetOption '..z..' SolidColor "#ColorSquareMineDefused#"')
		SKIN:Bang('!SetOption '..z..' ToolTipTitle "Defused Mine"') 
		SKIN:Bang('!SetOption '..z..' ToolTipText "You defused this mine. Nicely done."')
		SKIN:Bang('!SetOption '..z..' MouseOverAction "[]"')
		SKIN:Bang('!SetOption '..z..' MouseLeaveAction "[]"')
	elseif sRequest == 'TrippedMine' then
		SKIN:Bang('!SetOption '..z..' SolidColor "#ColorSquareMineTripped#"')
		SKIN:Bang('!SetOption '..z..' ToolTipTitle "Tripped Mine"') 
		SKIN:Bang('!SetOption '..z..' ToolTipText "You stepped on this mine. It\'s ok. Lots of people live without legs."')
		SKIN:Bang('!SetOption '..z..' MouseOverAction "[]"')
		SKIN:Bang('!SetOption '..z..' MouseLeaveAction "[]"')
	elseif sRequest == 'UntrippedMine' then
		SKIN:Bang('!SetOption '..z..' SolidColor "#ColorSquareMine#"') 
		SKIN:Bang('!SetOption '..z..' ToolTipTitle "Mine"') 
		SKIN:Bang('!SetOption '..z..' ToolTipText "There was a mine here."')
		SKIN:Bang('!SetOption '..z..' MouseOverAction """[!SetOption #*CURRENTSECTION*# SolidColor "255,0,0"][!Update]"""')
		SKIN:Bang('!SetOption '..z..' MouseLeaveAction """[!SetOption #*CURRENTSECTION*# SolidColor "#ColorSquareMine#"][!Update]"""')
	elseif sRequest == 'RightFlag' then
		SKIN:Bang('!SetOption '..z..' ToolTipTitle "Flag"') 
		SKIN:Bang('!SetOption '..z..' ToolTipText "You correctly identified this mine. Or, you just got lucky. But we won\'t hold it against you."')
		SKIN:Bang('!SetOption '..z..' MouseOverAction """[!SetOption #*CURRENTSECTION*# SolidColor "255,0,0"][!Update]"""')
		SKIN:Bang('!SetOption '..z..' MouseLeaveAction """[!SetOption #*CURRENTSECTION*# SolidColor "#ColorSquareFlag#"][!Update]"""')
	elseif sRequest == 'WrongFlag' then
		SKIN:Bang('!SetOption '..z..' SolidColor "#ColorSquareFlagWrong#"') 
		SKIN:Bang('!SetOption '..z..' ToolTipTitle "False Positive"') 
		SKIN:Bang('!SetOption '..z..' ToolTipText "You flagged this spot, but there was no mine here."')
		SKIN:Bang('!SetOption '..z..' MouseOverAction """[!SetOption #*CURRENTSECTION*# SolidColor "#ColorSquareClear#"][!Update]"""')
		SKIN:Bang('!SetOption '..z..' MouseLeaveAction """[!SetOption #*CURRENTSECTION*# SolidColor "#ColorSquareFlagWrong#"][!Update]"""')
	elseif sRequest == 'Remainder' then
		SKIN:Bang('!SetOption '..z..' SolidColor "#ColorSquareRevealed#"')
		SKIN:Bang('!SetOption '..z..' MouseOverAction """[!SetOption #*CURRENTSECTION*# SolidColor "#ColorSquareClear#"][!Update]"""')
		SKIN:Bang('!SetOption '..z..' MouseLeaveAction """[!SetOption #*CURRENTSECTION*# SolidColor "#ColorSquareRevealed#"][!Update]"""')
	end
end

function Message(sRequest)
	SKIN:Bang('!SetOption Message FontColor "#ColorText#"')
	SKIN:Bang('!SetOption Message Text ""')
	SKIN:Bang('!HideMeterGroup MessageConfirm')
	if sRequest == 'Close' then
		SKIN:Bang('!HideMeterGroup Message')
		return
	elseif sRequest == 'Suicide' then
		SKIN:Bang('!SetOption Message FontColor "#ColorSquareMineTripped#"')
		SKIN:Bang('!SetOption Message Text "I\'m sorry, Dave. I can\'t let you do that."')
	elseif sRequest == 'TooFewFlags' then
		SKIN:Bang('!SetOption Message FontColor "#ColorSquareMineTripped#"')
		SKIN:Bang('!SetOption Message Text "Too few flags."')
	elseif sRequest == 'TooFewFlagsAll' then
		SKIN:Bang('!SetOption Message FontColor "#ColorSquareMineTripped#"')
		SKIN:Bang('!SetOption Message Text "You still have '..iFlags..' unflagged mines."')
	elseif sRequest == 'Win' then
		SKIN:Bang('!SetOption Message FontColor "#ColorSquareMineDefused#"')
		SKIN:Bang('!SetOption Message Text "You won!"')
	elseif sRequest == 'Lose' then
		SKIN:Bang('!SetOption Message FontColor "#ColorSquareMineTripped#"')
		SKIN:Bang('!SetOption Message Text "Yeah, that was a mine. Sorry."')
	elseif sRequest == 'TooFewRows' then
		SKIN:Bang('!SetOption Message FontColor "#ColorSquareMineTripped#"')
		SKIN:Bang('!SetOption Message Text "You need at least 9 rows."')
	elseif sRequest == 'TooManyRows' then
		SKIN:Bang('!SetOption Message FontColor "#ColorSquareMineTripped#"')
		SKIN:Bang('!SetOption Message Text "You can only have 24 rows."')
	elseif sRequest == 'TooFewCols' then
		SKIN:Bang('!SetOption Message FontColor "#ColorSquareMineTripped#"')
		SKIN:Bang('!SetOption Message Text "You need at least 9 columns."')
	elseif sRequest == 'TooManyCols' then
		SKIN:Bang('!SetOption Message FontColor "#ColorSquareMineTripped#"')
		SKIN:Bang('!SetOption Message Text "You can only have 30 columns."')
	elseif sRequest == 'TooFewMines' then
		SKIN:Bang('!SetOption Message FontColor "#ColorSquareMineTripped#"')
		SKIN:Bang('!SetOption Message Text "You need at least 1 mine."')
	elseif sRequest == 'TooManyMines' then
		SKIN:Bang('!SetOption Message FontColor "#ColorSquareMineTripped#"')
		SKIN:Bang('!SetOption Message Text "You can\'t have more mines than squares."')
	elseif sRequest == 'ResetConfirm' then
		SKIN:Bang('!SetOption Message Text "Are you sure?"')
		SKIN:Bang('!ShowMeterGroup MessageConfirm')
	end
	SKIN:Bang('!ShowMeterGroup Message')
end

----------------------------------------------------------------------------------------------

function Settings(sSetting, sInput)
	local iInput = tonumber(sInput)
	local iWriteNumberOfRows = tonumber(SKIN:GetVariable('WriteNumberOfRows'))
	local iWriteNumberOfCols = tonumber(SKIN:GetVariable('WriteNumberOfCols'))
	local iWriteNumberOfMines = tonumber(SKIN:GetVariable('WriteNumberOfMines'))
	local iWriteQuestions = tonumber(SKIN:GetVariable('WriteQuestions'))
	
	if sSetting == 'Rows' then
		if iInput < 9 then
			iWriteNumberOfRows = 9
			Message('TooFewRows')
		elseif iInput > 24 then
			iWriteNumberOfRows = 24
			Message('TooManyRows')
		else
			iWriteNumberOfRows = iInput
			Message('Close')
		end
		SKIN:Bang('!SetVariable WriteNumberOfRows '..iWriteNumberOfRows)
		if iWriteNumberOfMines > iWriteNumberOfRows * iWriteNumberOfCols - 1 then
			iWriteNumberOfMines = iWriteNumberOfRows * iWriteNumberOfCols - 1
			SKIN:Bang('!SetVariable WriteNumberOfMines '..iWriteNumberOfMines)
		end
		
	elseif sSetting == 'Cols' then
		if iInput < 9 then
			iWriteNumberOfCols = 9
			Message('TooFewCols')
		elseif iInput > 30 then
			iWriteNumberOfCols = 30
			Message('TooManyCols')
		else
			iWriteNumberOfCols = iInput
			Message('Close')
		end
		SKIN:Bang('!SetVariable WriteNumberOfCols '..iWriteNumberOfCols)
		if iWriteNumberOfMines > iWriteNumberOfRows * iWriteNumberOfCols - 1 then
			iWriteNumberOfMines = iWriteNumberOfRows * iWriteNumberOfCols - 1
			SKIN:Bang('!SetVariable WriteNumberOfMines '..iWriteNumberOfMines)
		end
		
	elseif sSetting == 'Mines' then
		msMaxMines = SKIN:GetMeasure('MeasureCalcMaxMines')
		iMaxMines = tonumber(msMaxMines:GetStringValue())
		if iInput < 1 then
			iWriteNumberOfMines = 1
			Message('TooFewMines')
		elseif iInput > iMaxMines then
			iWriteNumberOfMines = iMaxMines
			Message('TooManyMines')
		else
			iWriteNumberOfMines = iInput
			Message('Close')
		end
		SKIN:Bang('!SetVariable WriteNumberOfMines '..iWriteNumberOfMines)
		
	elseif sSetting == 'Apply' then
		local iSquareSize = tonumber(SKIN:GetVariable('SquareSize'))
		local iSquareMargin = tonumber(SKIN:GetVariable('SquareMargin'))
		local iSkinWidth = (iSquareSize + iSquareMargin) * iWriteNumberOfCols + iSquareMargin
		local iSkinHeight = (iSquareSize + iSquareMargin) * iWriteNumberOfRows + iSquareMargin + 70
		
		local iWriteInputX = iSkinWidth - 118
		local iWriteInputY1 = iSkinHeight - 234
		local iWriteInputY2 = iWriteInputY1 + 36
		local iWriteInputY3 = iWriteInputY2 + 36
		
		SKIN:Bang('!WriteKeyValue Variables NumberOfRows '..iWriteNumberOfRows..' "#CURRENTPATH#Settings.inc"')
		SKIN:Bang('!WriteKeyValue Variables NumberOfCols '..iWriteNumberOfCols..' "#CURRENTPATH#Settings.inc"')
		SKIN:Bang('!WriteKeyValue Variables NumberOfMines '..iWriteNumberOfMines..' "#CURRENTPATH#Settings.inc"')
		SKIN:Bang('!WriteKeyValue Variables Questions '..iWriteQuestions..' "#CURRENTPATH#Settings.inc"')
		SKIN:Bang('!WriteKeyValue Variables InputX '..iWriteInputX..' "#CURRENTPATH#Settings.inc"')
		SKIN:Bang('!WriteKeyValue Variables InputY1 '..iWriteInputY1..' "#CURRENTPATH#Settings.inc"')
		SKIN:Bang('!WriteKeyValue Variables InputY2 '..iWriteInputY2..' "#CURRENTPATH#Settings.inc"')
		SKIN:Bang('!WriteKeyValue Variables InputY3 '..iWriteInputY3..' "#CURRENTPATH#Settings.inc"')
		return
	end
	
	local sWriteLevel = DetectLevel(iWriteNumberOfRows, iWriteNumberOfCols, iWriteNumberOfMines)
	local sColorLevel1 = sWriteLevel == 'Beginner'     and '#ColorSquareFlag#' or '#ColorTextDim#'
	local sColorLevel2 = sWriteLevel == 'Intermediate' and '#ColorSquareFlag#' or '#ColorTextDim#'
	local sColorLevel3 = sWriteLevel == 'Advanced'     and '#ColorSquareFlag#' or '#ColorTextDim#'
	SKIN:Bang('!SetOption SettingsBeginner FontColor "'..sColorLevel1..'"')
	SKIN:Bang('!SetOption SettingsIntermediate FontColor "'..sColorLevel2..'"')
	SKIN:Bang('!SetOption SettingsAdvanced FontColor "'..sColorLevel3..'"')
end

--------------------------------------------------------------------------------------------

function WriteSetVariable(sKey, sValue, sFile)
	SKIN:Bang('!SetVariable "'..sKey..'" "'..sValue..'"')
	SKIN:Bang('!WriteKeyValue Variables "'..sKey..'" "'..sValue..'" "'..sFile..'"')
end

function Scores(sShowLevel, sRequest)
	if sShowLevel ~= 'Custom' then
		local iPlayed = tonumber(SKIN:GetVariable(sShowLevel..'Played'))
		local iWon = tonumber(SKIN:GetVariable(sShowLevel..'Won'))
		local iStreak = tonumber(SKIN:GetVariable(sShowLevel..'Streak'))
		local iMostWins = tonumber(SKIN:GetVariable(sShowLevel..'MostWins'))
		local iMostLosses = tonumber(SKIN:GetVariable(sShowLevel..'MostLosses'))
		local tScores = {}
		for i = 1,5 do
			tScores[i] = {}
			tScores[i]['time'] = tonumber(SKIN:GetVariable(sShowLevel..'Time'..i))
			tScores[i]['date'] = SKIN:GetVariable(sShowLevel..'Date'..i)
		end
		
		if sRequest then
			local sCurrentPath = SKIN:GetVariable('CURRENTPATH')
			local sSettings = sCurrentPath..'Settings.inc'
			if sRequest == 'Update' then
				
				
				iPlayed = iPlayed + 1
				
				if iGameOver == 1 then
					iWon = iWon + 1
					if iStreak > 0 then
						iStreak = iStreak + 1
					else
						iStreak = 1
					end
					if iStreak > iMostWins then iMostWins = iStreak end
					if iTimer < tScores[5]['time'] then
						msDate = SKIN:GetMeasure('MeasureDate')
						sDate = msDate:GetStringValue()
						table.insert(tScores, { ['time']=iTimer, ['date']=sDate })
						table.sort(tScores, function(a,b) return a['time'] < b['time'] end)
					end
				else
					if iStreak < 0 then
						iStreak = iStreak - 1
					else
						iStreak = -1
					end
					if math.abs(iStreak) > iMostLosses then iMostLosses = math.abs(iStreak) end
					
				end
			
			elseif sRequest == 'Reset' then
				iPlayed = 0
				iWon = 0
				iStreak = 0
				iMostWins = 0
				iMostLosses = 0
				for i = 1,5 do
					tScores[i]['time'] = 999
					tScores[i]['date'] = 'Never'
				end
			end
			
			WriteSetVariable(sShowLevel..'Played', iPlayed, sSettings)
			WriteSetVariable(sShowLevel..'Won', iWon, sSettings)
			WriteSetVariable(sShowLevel..'Streak', iStreak, sSettings)
			WriteSetVariable(sShowLevel..'MostWins', iMostWins, sSettings)
			WriteSetVariable(sShowLevel..'MostLosses', iMostLosses, sSettings)
			for i = 1,5 do
				WriteSetVariable(sShowLevel..'Time'..i, tScores[i]['time'], sSettings)
				WriteSetVariable(sShowLevel..'Date'..i, tScores[i]['date'], sSettings)
			end
		end
		
		iPercent = iPlayed > 0 and math.ceil((iWon / iPlayed) * 10000) / 100 or 0
		
		SKIN:Bang('!SetOption ScoresPlayedValue Text '..iPlayed)
		SKIN:Bang('!SetOption ScoresWonValue Text "'..iPercent..'%"')
		SKIN:Bang('!SetOption ScoresWonValue ToolTipText "'..iWon..' of '..iPlayed)
		SKIN:Bang('!SetOption ScoresStreakValue Text '..iStreak)
		SKIN:Bang('!SetOption ScoresMostWinsValue Text '..iMostWins)
		SKIN:Bang('!SetOption ScoresMostLossesValue Text '..iMostLosses)
		SKIN:Bang('!SetOption ScoresBestTimeValue Text '..tScores[1]['time'])
		SKIN:Bang('!SetOption ScoresBestTimeValue ToolTipText "'..tScores[1]['time']..'#CRLF#'..tScores[1]['date']..'#CRLF##CRLF#'..tScores[2]['time']..'#CRLF#'..tScores[2]['date']..'#CRLF##CRLF#'..tScores[3]['time']..'#CRLF#'..tScores[3]['date']..'#CRLF##CRLF#'..tScores[4]['time']..'#CRLF#'..tScores[4]['date']..'#CRLF##CRLF#'..tScores[5]['time']..'#CRLF#'..tScores[5]['date']..'"')
		SKIN:Bang('!SetOption ScoresReset FontColor "#ColorTextBright#"')
		SKIN:Bang('!SetOption MessageYes LeftMouseUpAction """[!CommandMeasure MeasureScript Message(\'Close\')][!CommandMeasure MeasureScript "Scores(\''..sShowLevel..'\', \'Reset\')"][!Update]"""')
		SKIN:Bang('!SetOption ScoresReset ToolTipText "Click to reset your statistics for the '..sShowLevel..' level."')
	end
	
	local sColorLevel1 = sShowLevel == 'Beginner'     and '#ColorSquareFlag#' or '#ColorTextDim#'
	local sColorLevel2 = sShowLevel == 'Intermediate' and '#ColorSquareFlag#' or '#ColorTextDim#'
	local sColorLevel3 = sShowLevel == 'Advanced'     and '#ColorSquareFlag#' or '#ColorTextDim#'
	SKIN:Bang('!SetOption ScoresBeginner FontColor "'..sColorLevel1..'"')
	SKIN:Bang('!SetOption ScoresIntermediate FontColor "'..sColorLevel2..'"')
	SKIN:Bang('!SetOption ScoresAdvanced FontColor "'..sColorLevel3..'"')
end