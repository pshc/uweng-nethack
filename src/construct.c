#include "hack.h"

static boolean
construct_check(verbose, x, y)
	boolean		verbose;
	int		x, y;
{
	struct rm *lev = &levl[x][y];
	struct trap *ttmp = t_at(x, y);

	if (On_stairs(x, y)) {
	    if (x == xdnladder || x == xupladder) {
		if(verbose) pline_The("ladder resists your effort.");
	    } else if(verbose) pline_The("stairs resist your effort.");
	} else if (IS_THRONE(lev->typ)) {
	    if(verbose) pline_The("throne resists!");
	    exercise(A_INT, FALSE);
	} else if (IS_ALTAR(lev->typ)) {
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
	} else if (In_sokoban(&u.uz)) {
	    if(verbose) pline("A mysterious force prevents construction!");
	}
	else if (lev->typ == CORR || lev->typ == ROOM || lev->typ == TREE) {
	    return TRUE; /* Just allow these types for now */
	}
	else if (verbose) {
	    You("cannot construct there.");
	}
	return FALSE;
}

static boolean
construct_door_ok(x, y)
	int x, y;
{
    if (levl[x][y].typ == CORR) {
	return TRUE;
    }
    else if (x > 0 && x < COLNO-1 && IS_STWALL(levl[x-1][y].typ)
		    && IS_STWALL(levl[x+1][y].typ)) {
	return TRUE;
    }
    else if (y > 0 && y < ROWNO-1 && IS_STWALL(levl[x][y-1].typ)
		    && IS_STWALL(levl[x][y+1].typ)) {
	return TRUE;
    }
    return FALSE;
}

struct constructed_trap {
    int kind;
    const char *name;
};

static const struct constructed_trap up_traps[] = {
	/*{DART_TRAP, "dart trap"}, {ARROW_TRAP, "arrow trap"},*/
	{ROCKTRAP, "falling rock trap"},
	};
/* Holes are not a good idea for certain levels... */
static const struct constructed_trap down_traps[] = {
	{PIT, "pit"}, {SPIKED_PIT, "spiked pit"},
	};

/* TODO: Don't use a charge for failed attempts */
void
zap_construct()
{
	struct rm *room;
	struct monst *mtmp;
	struct obj *otmp;
	const struct constructed_trap *trap;
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

	    if (!construct_check(TRUE, u.ux, u.uy))
	    	return;

	    trap = u.dz < 0 ? &up_traps[rn2(SIZE(up_traps))]
	                    : &down_traps[rn2(SIZE(down_traps))];
	    seetrap(maketrap(u.ux, u.uy, trap->kind));
	    You("construct a %s in the %s.", trap->name,
		    u.dz < 0 ? ceiling(u.ux, u.uy) : surface(u.ux, u.uy));
	    return;
	} /* up or down */

	/* normal case: construct next to you */
	zx = u.ux + u.dx;
	zy = u.uy + u.dy;
	if (!construct_check(TRUE, zx, zy))
		return;
	room = &levl[zx][zy];
	/* Pretty display */
	tmp_at(DISP_BEAM, cmap_to_glyph(S_digbeam));
	delay_output();
	tmp_at(zx, zy);
	otmp = level.objects[zx][zy];
	if (otmp) {
	    if (otmp->nexthere) {
		pline("You're not sure what to do with these objects.");
	    } else switch (otmp->otyp) {
	    case BOULDER:
		if(!cansee(zx, zy))
			You_hear("rumbling.");
		else
			pline_The(room->typ == CORR ? "%s the hallway."
				: "%s and connects to the ceiling.",
				aobjnam(otmp, room->typ ==
					CORR ? "fill" : "expand"));
		remove_object(otmp);
		room->typ = STONE;
		break;
	    case IRON_CHAIN:
		if (otmp == uchain) {
			pline("%s but nothing happens.",
				The(aobjnam(otmp, "shudder")));
			break;
		}
		if (!cansee(zx, zy))
			You_hear("clanging metal.");
		else
			pline("%s and the links morph into bars.",
				The(aobjnam(otmp, "unravel")));
		remove_object(otmp);
		room->typ = IRONBARS;
		break;
	    case DART:
		remove_object(otmp);
		seetrap(maketrap(zx, zy, DART_TRAP));
		You("assemble a dart trap.");
		break;
	    case ARROW:
	    case ELVEN_ARROW:
	    case ORCISH_ARROW:
	    case SILVER_ARROW:
	    case YA:
	    case CROSSBOW_BOLT:
		remove_object(otmp);
		seetrap(maketrap(zx, zy, ARROW_TRAP));
		You("assemble an arrow trap.");
		break;
	    case WAN_SLEEP:
		if (!otmp->spe) goto no_charges;
	    case POT_SLEEPING:
	    case SPE_SLEEP:
	    case AMULET_OF_RESTFUL_SLEEP:
		remove_object(otmp);
		seetrap(maketrap(zx, zy, SLP_GAS_TRAP));
		You("set up a sleeping gas trap.");
		break;
	    case FIRE_HORN:
	    case WAN_FIRE:
		if (!otmp->spe) goto no_charges;
	    case SCR_FIRE:
	    case SPE_FIREBALL:
		remove_object(otmp);
		seetrap(maketrap(zx, zy, FIRE_TRAP));
		You("set up a fire trap.");
		break;
	    case WAN_POLYMORPH:
		if (!otmp->spe) goto no_charges;
	    case POT_POLYMORPH:
	    case RIN_POLYMORPH:
	    case SPE_POLYMORPH:
		remove_object(otmp);
		seetrap(maketrap(zx, zy, POLY_TRAP));
		You("set up a polymorph trap.");
		break;
	    case WAN_TELEPORTATION:
		if (!otmp->spe) goto no_charges;
	    case SCR_TELEPORTATION:
	    case RIN_TELEPORTATION:
	    case SPE_TELEPORT_AWAY:
		remove_object(otmp);
		seetrap(maketrap(zx, zy, TELEP_TRAP));
		You("set up a teleportation trap.");
		break;
	    case WAN_CANCELLATION:
		if (!otmp->spe) goto no_charges;
	    case SPE_CANCELLATION:
		remove_object(otmp);
		seetrap(maketrap(zx, zy, ANTI_MAGIC));
		You("set up an anti-magic trap.");
		break;
	    default:
		You("cannot decide what to make with %s.", the(xname(otmp)));
		break;
	    no_charges:
		pline("%s has no charges! Your trap fails.", The(xname(otmp)));
	    }
	} else if (construct_door_ok(zx, zy)) {
		/* make a door */
		room->typ = DOOR;
		room->doormask = D_CLOSED;
		if (cansee(zx, zy))
			pline("A door suddenly appears!");
		else
			You_hear("a door close.");
	} else if (IS_TREE(room->typ)) {
		/* Turn trees into a chest */
		room->typ = ROOM;
		otmp = mksobj_at(CHEST, zx, zy, FALSE, FALSE);
		if(otmp) {
			(void)xname(otmp);
			stackobj(otmp);
			mkbox_cnts(otmp); /* Too nice? */
			if (cansee(zx, zy))
			    pline_The("tree becomes %s!", an(xname(otmp)));
			else
			    You_hear("a loud clunk.");
		}
	} else {
		/* default case */
		room->typ = FOUNTAIN;
		room->blessedftn = 0;
		level.flags.nfountains++;
		if (cansee(zx, zy))
			pline("A fountain suddenly appears!");
		else
			You_hear("bubbling water.");
	}
	tmp_at(DISP_END,0);	/* closing call */
	newsym(zx, zy);
}

/*construct.c*/
