// ---------------- 2. STIMULUS SETUP ------------------
// Condition - call the maker getter to get the cond variable 
// Parameters and Stimulus Setup 

// I'm still working on figuring out how exactly this part works,
// but Mickael explained that "var condCounts = "1,75;2,75;3,75;4,75""
// is meant to cycle through four conditions (as of yet I don't get)
// which conditions these are supposed to be. 
try {
    var filename = "MCF_pragmods_v6"
    var condCounts = "1,75;2,75;3,75;4,75"
    var xmlHttp = null;
    xmlHttp = new XMLHttpRequest();
    xmlHttp.open( "GET", "http://langcog.stanford.edu/cgi-bin/subject_equalizer/maker_getter.php?conds=" + 
		  condCounts +"&filename=" + filename, false );
    xmlHttp.send( null );
    var cond = xmlHttp.responseText;
} catch (e) {
    var cond = 1;
}

// CROSS CONDITIONS
// familiarization conditions
var fam_dists = [[0, 1, 2, 2, 2, 2, 2, 2, 2],
        [0, 1, 1, 1, 2, 2, 2, 2, 2],
        [0, 1, 1, 1, 1, 1, 2, 2, 2],
        [0, 1, 1, 1, 1, 1, 1, 1, 2]];
var target_frequencies = [.125,.375,.625,.875]; // What is being hardcoded here???
var num_fam = 9;  // What is being hardcoded here???
var num_fam_conds = 4; // What is being hardcoded here??? - Looks very important to the task of setting experiment conditions.


// This is specific to the experiment version - probably very relevant for making
// a general experiment maker. If I understand it right 

/*
if (cond < num_fam_conds + 1) {
    var word_cond = "baserate";
    var fam_cond = cond - 1; // subtract 1 to convert from 1 to 0
} else if (cond >= num_fam_conds) {
    var word_cond = "salience";
    var fam_cond = cond - (num_fam_conds) - 1; // subtract 1 to convert from 1 to 0 
}
*/

var word_cond = "baserate";  // The type of experiment that is being conducted, I suppose?
var fam_cond = cond - 1; // subtract 1 to convert from 1 to 0. Sure but why?



var fam_dist = fam_dists[fam_cond];

// bookkeeping variables
var choices = [0, 1, 2];
var target = -2;
var fam_clicked = new Array();
var fam_finished;
var positions = ["left","middle","right"];

// ADAPTED FROM PRAGMODS R CODE
var choice_names_unpermuted = ["foil","target","logical"];

var expt = [[1, 0, 0], [1, 1, 0], [0, 1, 1]]; // NO SCALES
var level = 2;
var target_unpermuted = 1;
var distractor_unpermuted = 2;
var other_unpermuted = 0;
var target_prop_unpermuted = 1;
var distractor_prop_unpermuted = 0;

/*
var expt = [[0, 0, 0], [0, 0, 1], [0, 1, 1]]; // SCALES
var level = 1;
var target_unpermuted = 1;
var distractor_unpermuted = 2;
var other_unpermuted = 0;
var target_prop_unpermuted = 2;
var distractor_prop_unpermuted = 1;
*/

var stims = ["bike","friend","pizza","snowman","sundae","Christmas tree"];
var stims_plural = ["bikes","friends","pizzas","snowmen","sundaes","Christmas trees"];

var stims_props = [["basket","waterbottle","saddlebag"],
		   ["hat","glasses","mustache"],
		   ["mushrooms","olives","peppers"],
                   ["hat","scarf","mittens"],		   
                   ["cherry","whipped cream","chocolate"],
		   ["lights","ornaments","star"]];
var stims_prop_words = [["a basket","a water bottle","saddlebags"],
			["a hat","glasses","a mustache"],
			["mushrooms","olives","peppers"],
                        ["a hat","a scarf","mittens"],
                        ["a cherry","whipped cream","chocolate sauce"],
			["lights","ornaments","a star"]];
var stims_actions = [["ride","rents","rode"],
		     ["visit","chooses to visit","visited"],
		     ["eat","orders","ate"],
		     ["decorate","makes","decorated"],
		     ["eat","makes","ate"],
		     ["trim","buys","trimmed"]];
var stims_times = [["weekend","Week"],
		   ["Sunday","Week"],
		   ["Wednesday","Week"],
		   ["winter","Year"],
		   ["Friday","Week"],
		   ["Christmas","Year"]];

// Elaborate on the purpose of this. Which image is being changed
var img_size = 200; // needs to be implemented, currently just a placeholder   
var stim_index = random(0,stims.length-1);

// Permute the matrix randomly:
var prop_perm = shuffle(range(0,expt[0].length-1));
var target_perm = shuffle(range(0,expt.length-1));
var expt_perm = new Array();
var choice_names = new Array();

for (var i=0; i<expt.length; i++) {
    expt_perm[i] = new Array()
    for (var j=0; j<expt[0].length; j++) {
	expt_perm[i][j] = expt[target_perm[i]][prop_perm[j]];
    }
    choice_names[i] = choice_names_unpermuted[target_perm[i]];
}

var base = stims[stim_index];
var plural = stims_plural[stim_index];
var actions = stims_actions[stim_index];
var props = stims_props[stim_index];
var prop_words = stims_prop_words[stim_index];
var times = stims_times[stim_index];

var target = target_perm.indexOf(target_unpermuted);
var distractor = target_perm.indexOf(distractor_unpermuted);
var other = target_perm.indexOf(other_unpermuted);
var target_prop = prop_perm.indexOf(target_prop_unpermuted);
var distractor_prop = prop_perm.indexOf(distractor_prop_unpermuted);

// create shuffled familiarization
fam_mat = new Array();
fam_perm = shuffle(range(0,fam_dist.length-1))
for (var i = 0; i < num_fam; i++) {
    fam_mat[i] = new Array();
    for (var j = 0; j < expt[0].length; j++) {
	fam_mat[i][j] = expt[fam_dist[fam_perm[i]]][prop_perm[j]];
    }
}
