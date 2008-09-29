/* Copyright (c) Stichting Mathematisch Centrum, Amsterdam, 1985. */
/* NetHack may be freely redistributed.  See license for details. */

#include "hack.h"
/* #define DEBUG */	/* turn on for diagnostics */

/* These monster classes may not evolve unless an exception is present
 * in grownups[] */
static short non_evolving[] = {
	S_ANGEL,
	S_COCKATRICE,
	S_DEMON,
	S_DOG,
	S_DRAGON,
	S_ELEMENTAL,
	S_FELINE,
	S_GOLEM,
	S_HUMAN,
	S_HUMANOID,
	S_MUMMY,
	S_NAGA,
	S_UNICORN,
	S_WORM,
	S_VAMPIRE,
	S_WRAITH,
	S_ZOMBIE,
	0
};

struct permonst *
mon_evolves_into(mtmp, devolve)
	struct monst *mtmp;
	boolean devolve;
{
    struct permonst *cls = mtmp->data, *adj;
    int i;

    /* Special exceptions? */
    for (i = 0; grownups[i][0] >= LOW_PM; i++)
	if (cls == &mons[grownups[i][devolve]])
	    return &mons[grownups[i][!devolve]];

    for (i = 0; non_evolving[i]; i++)
	if (cls->mlet == non_evolving[i])
	    return NULL;

    /* Otherwise, just evolve to the next (or previous) on the monster list
     * if it's of the same class */
    adj = devolve ? cls - 1 : cls + 1;
    if (adj < mons || adj->mlet != cls->mlet)
	return NULL;

    return adj;
}

short /* otyp */
obj_evolves_into(otmp, devolve)
	struct obj *otmp;
	boolean devolve;
{
	return 0;
}

boolean
evolve_mon(mtmp, devolve)
	struct monst *mtmp;
	boolean devolve;
{
    struct permonst *target = mon_evolves_into(mtmp, devolve);
    char buf[BUFSZ], buf2[BUFSZ];
    char *cp, *name = buf, *new_name = buf2;
    boolean self = (mtmp == &youmonst);
    boolean success = FALSE;

    if (self)
	strcpy(name, plname);
    else {
    	strcpy(name, Monnam(mtmp));
	if (!strncmpi(name, "the ", 4))
	    name += 4;
    }
    if (strcmp(name, "It"))
	for (cp = name; *cp; cp++)
	    *cp = highc(*cp);
    pline("%s is %sevolving!", name, devolve ? "d" : "");
    if (target && (!self || !Polymorph_control
			 || yn(devolve ? "Devolve?" : "Evolve?") == 'y')) {
	if (!self)
	    success = newcham(mtmp, target, TRUE, FALSE);
	else {/* There's gotta be a better way to do this */
	    success = polymon(target - mons);
	}
    }
    if (success) {
	strcpy(new_name, target->mname);
	for (cp = new_name; *cp; cp++)
	    *cp = highc(*cp);
	pline("%s %sevolved into %s!", name, devolve ? "d" : "", new_name);
    }
    else
	pline("But it had no effect!");
    return success;
}

struct obj *
evolve_obj(otmp, devolve)
	struct obj *otmp;
	boolean devolve;
{
    /* deferred */
    return otmp;
}

void
evolveself(devolve)
	boolean devolve;
{
    evolve_mon(&youmonst, devolve);
}

/*evolve.c*/
