/*	SCCS Id: @(#)dig.c	3.4	2003/03/23	*/
/* Copyright (c) Stichting Mathematisch Centrum, Amsterdam, 1985. */
/* NetHack may be freely redistributed.  See license for details. */

#include "hack.h"
#include "edog.h"
/* #define DEBUG */	/* turn on for diagnostics */

/*
boolean
is_constructing()
{
	if (occupation == construct) {
	    return TRUE;
	}
	return FALSE;
}
*/

#define BY_YOU		(&youmonst)
#define BY_OBJECT	((struct monst *)0)

boolean
construct_check(madeby, verbose, x, y)
	struct monst	*madeby;
	boolean		verbose;
	int		x, y;
{
	struct rm *lev = &levl[x][y];
	struct trap *ttmp = t_at(x, y);

	if (On_stairs(x, y)) {
	    if (x == xdnladder || x == xupladder) {
		if(verbose) pline_The("ladder resists your effort.");
	    } else if(verbose) pline_The("stairs resist your effort.");
	} else if (IS_THRONE(lev->typ)) /* && madeby != BY_OBJECT)*/ {
	    if(verbose) pline_The("throne resists!");
	    exercise(A_INT, FALSE);
	} else if (IS_ALTAR(lev->typ)) /* && (madeby != BY_OBJECT ||
				Is_astralevel(&u.uz) || Is_sanctum(&u.uz)))*/ {
	    if(verbose) pline_The("altar resists!");
	    exercise(A_WIS, FALSE);
	} else if (Is_airlevel(&u.uz)) {
	    if(verbose) You("cannot construct things in thin air!");
	} else if (Is_waterlevel(&u.uz)) {
	    if(verbose) pline_The("water splashes and subsides.");
	} else if (IS_WALL(lev->typ)) {
	    if(verbose) pline_The("wall here is in the way.");
	} else if (MON_AT(x, y)) {
	    if(verbose) pline("There's no room to do that!");
	} else if (ttmp && ttmp->ttyp == MAGIC_PORTAL) {
	    if(verbose) pline("A portal is in the way.");
	}
	else {
		return(TRUE);
	}
	return(FALSE);
}

/* When will hole be finished? Very rough indication used by shopkeeper. */
int
constructtime()
{
	/*if(occupation != construct || !*u.ushops) return(-1);
	return ((250 - digging.effort) / 20);*/
	return 20;
}

/*
 * Town Watchmen frown on damage to the town walls, trees or fountains.
 * It's OK to dig holes in the ground, however.
 * If mtmp is assumed to be a watchman, a watchman is found if mtmp == 0
 * zap == TRUE if wand/spell of digging, FALSE otherwise (chewing)
 */
/*
void
watch_dig(mtmp, x, y, zap)
    struct monst *mtmp;
    xchar x, y;
    boolean zap;
{
	struct rm *lev = &levl[x][y];

	if (in_town(x, y) &&
	    (closed_door(x, y) || lev->typ == SDOOR ||
	     IS_WALL(lev->typ) || IS_FOUNTAIN(lev->typ) || IS_TREE(lev->typ))) {
	    if (!mtmp) {
		for(mtmp = fmon; mtmp; mtmp = mtmp->nmon) {
		    if (DEADMONSTER(mtmp)) continue;
		    if ((mtmp->data == &mons[PM_WATCHMAN] ||
			 mtmp->data == &mons[PM_WATCH_CAPTAIN]) &&
			mtmp->mcansee && m_canseeu(mtmp) &&
			couldsee(mtmp->mx, mtmp->my) && mtmp->mpeaceful)
			break;
		}
	    }

	    if (mtmp) {
		if(zap || digging.warned) {
		    verbalize("Halt, vandal!  You're under arrest!");
		    (void) angry_guards(!(flags.soundok));
		} else {
		    const char *str;

		    if (IS_DOOR(lev->typ))
			str = "door";
		    else if (IS_TREE(lev->typ))
			str = "tree";
		    else if (IS_ROCK(lev->typ))
			str = "wall";
		    else
			str = "fountain";
		    verbalize("Hey, stop damaging that %s!", str);
		    digging.warned = TRUE;
		}
		if (is_digging())
		    stop_occupation();
	    }
	}
}
*/

/* digging via wand zap or spell cast */
void
zap_construct()
{
	struct rm *room;
	struct monst *mtmp;
	struct obj *otmp;
	int zx, zy;

	if (u.uswallow) {
	    mtmp = u.ustuck;

	    if (!is_whirly(mtmp->data)) {
		if (is_animal(mtmp->data))
		    You("fill %s %s with bricks! It bursts!",
			s_suffix(mon_nam(mtmp)), mbodypart(mtmp, STOMACH));
		mtmp->mhp = 1;		/* almost dead */
		losehp(rnd(10), "bursted brick-filled stomach", KILLED_BY_AN);
		expels(mtmp, mtmp->data, !is_animal(mtmp->data));
	    }
	    return;
	} /* swallowed */


	if (u.dz) {
	    /* make a staircase in the given direction*/
	    /*if (!Is_airlevel(&u.uz) && !Is_waterlevel(&u.uz) && !Underwater) {*/
	    /*if (On_stairs(u.ux, u.uy)) {
	    	if(cansee(u.ux, u.uy))
	    	    pline_The("beam hits the %s and is absorbed.",
			(u.ux == xdnladder || u.ux == xupladder)
			? "ladder" : "stairs");
		return;
	    }*/
	    if (!construct_check(BY_YOU, TRUE, u.ux, u.uy))
	    	return;
	    /*pline_The("beam deflects harmlessly off the %s.",
	    		u.dz < 0 ? "floor" : "ceiling");
	    pline("Looks like that doesn't work yet.");*/
	    room = &levl[u.ux][u.uy];
	    room->typ = STAIRS;
	    room->ladder = u.dz < 0 ? LA_DOWN : LA_UP;
	    newsym(u.ux, u.uy);
	    /*otmp = mksobj_at(ROCK, u.ux, u.uy, FALSE, FALSE);
		    if (otmp) {
			(void)xname(otmp);
			stackobj(otmp);
		    }*/
	    return;
	} /* up or down */

	/* normal case: construct next to you */
	zx = u.ux + u.dx;
	zy = u.uy + u.dy;
	if (!construct_check(BY_YOU, TRUE, zx, zy))
		return;
	room = &levl[zx][zy];
	/* Pretty display */
	tmp_at(DISP_BEAM, cmap_to_glyph(S_digbeam)); /* TODO */
	delay_output();
	tmp_at(zx, zy);
	if (IS_TREE(room->typ)) {
		/* Turn trees into a chest */
		room->typ = ROOM;
		otmp = mksobj_at(CHEST, zx, zy, FALSE, FALSE);
		if(otmp) {
			(void)xname(otmp);
			stackobj(otmp);
			mkbox_cnts(otmp);
		}
		if(!cansee(zx, zy))
			pline("You hear a loud clunk.");
		else
			pline("The tree becomes a wooden chest!");
	} else if ((otmp = sobj_at(BOULDER, zx, zy)) != 0) {
		if(!cansee(zx, zy))
			pline("You hear rumbling.");
		else if(room->typ == CORR)
			pline("The boulder fills the hallway.");
		else
			pline("The boulder expands and connects to the ceiling.");
		remove_object(otmp);
		room->typ = STONE;
	} else {
		if(OBJ_AT(zx, zy)) {
			pline("Something is in the way!");
			return;
		}
		/* default case */
		if(rnd(10) > 5) {
			/* make a door */
			room->typ = DOOR;
			room->doormask = D_CLOSED;
			if (cansee(zx, zy))
				pline("A door suddenly appears!");
			else
				pline("You hear a door close.");
		} else {
			room->typ = FOUNTAIN;
			level.flags.nfountains++;
			/* TODO: set fountain status */
			if (cansee(zx, zy))
				pline("A fountain suddenly appears!");
			else
				pline("You hear bubbling water.");
		}
	}
	unblock_point(zx,zy); /* vision */
	tmp_at(DISP_END,0);	/* closing call */
	newsym(zx, zy);
}

/*construct.c*/
