
// ---------------- 3. CONTROL FLOW ------------------
// This .js file determines the flow of the variable elements in the experiment as dictated 
// by the various calls from pragmods html.

/*
Here the images used in the experiment are loaded in two arrays.
The first is base_image_pl, which stores the "underlying" or base images
which will then be modified with props stored in the props_image_pl Array.

NOTE: Unfortunately the number of variations for each type of object is HARDCODED.
To make the code more usable it will be necessary to 
*/

var base_image_pl = new Array();
for (i=0; i<3; i++) {
    base_image_pl[i] = new Image()
    base_image_pl[i].src = "images2/" + base + "-base" + String(i+1) + ".png" 
}


// By creating image object and setting source, the proloaded images become accesible. In this case they are stored as elements of the Array.
var props_image_pl = new Array() 
for (i=0;i<props.length;i++) {
    props_image_pl[i] = new Image()
    props_image_pl[i].src = "images2/" + base + "-" + props[i] + ".png" 
} 

showSlide("instructions");

// The main experiment:
//		The variable/object 'experiment' has two distinct but interrelated components:
//		1) It collects the variables that will be delivered to Turk at the end of the experiment
//		2) It hall all the functions that change the visual aspect of the experiment such as showing images, etc.

var experiment = {

    // These variables are the ones that will be sent to Turk at the end.
    // The first batch, however, is set determined by the experiment conditions
    // and therefore should not be affected by the decisions made by the experimental subject.
	item: base,
	target_property: props[target_prop],
	target_frequency: target_frequencies[fam_cond],
	target_position: positions[target],
	choice: "null",
	choice_correct: "FALSE",
	familiarization_cond: fam_cond, // This apparently is supposed to set the condition of the experiment. I.e. it will be settable from "outside"
	word_condition: word_cond, // As above

	// These are the variables that will be modified as a function of how the user responds to the experiment.
	// They are the ones necessary to do any science at all. 
	manip_check_target: 0,
	manip_check_dist: 0,
	name_check_correct: "FALSE",
	about: "",
	comment: "",

	// FAMILIARIZATION DISPLAY FUNCTION
	next_familiarization: function() {
	    // Allow experiment to start if it's a turk worker OR if it's a test run
	    // Warning, it may not be security best practices... you know why
	if (window.self == window.top | turk.workerId.length > 0) {
		// FAMILIARIZATION INSTRUCTIONS
		var familiarization_html = '<p class="block-text"">Bob really likes to ' + 
		actions[0] + ' ' + plural + '. ' +
		'Every ' + times[0] + ' he ' + actions[1] + ' a ' + base + '.<p>' +
		'<p class="block-text">Click on each ' + times[1].toLowerCase() + ' to see the ' + base + ' Bob ' + actions[2] + '.</p>'
		$("#familiarizationText").html(familiarization_html)

		// TIME BY TIME POPUPS FOR FAMILIARIZATION
		var fam_objects_html = '<table align="center"><tr>'

		for (i=0;i<=num_fam-1;i++){
			fam_objects_html += '<td width=200px height=230px align="center" ' +
				'class="objTable"' + 
				'id="famTable' + String(i) + 
				'" onclick="experiment.reveal(' + String(i) + ')">'
			fam_objects_html += times[1] + ' ' + String(i+1) + ', Bob ' + actions[2] + ':<div id="day' + String(i) + '"> </div>'
			if ((i+1)%3 == 0) {
				fam_objects_html += "</tr><tr>"
			}
		}
		fam_objects_html += '</tr></table>'
		$("#famObjects").html(fam_objects_html)
		showSlide("prestage");	
		}
	},

	// MAIN DISPLAY FUNCTION
	next_test: function() {

		showSlide("stage");

		// CREATE SETUP
		var setup_html = '<p class="block-text">Take a look at these ' + plural + '!</p>'
		$("#setup").html(setup_html) 	

		// CREATE OBJECT TABLE
		// (tr=table row; td= table data)
		var objects_html = '<table align="center"><tr>'

		// Q: Is this 3 a variable thing 
		for (i=0;i<3;i++){
			objects_html += '<td width=198px height=210px align="center"' + 
			' class="unchosen objTable" ' +
			'id="tdchoice' + String(i) + '" ' +
			'onclick=\"experiment.select(' + String(i) + ')\">'

			objects_html += stimHTML(base,i,expt_perm[i],props,'obj')
			objects_html += '</td>'
		}


		objects_html += '</tr><tr>'
		objects_html += '</tr></table>'

		$("#objects").html(objects_html) 


		// CREATE MANIPULATION CHECK COMMON GROUNDING
		var manipCheck_html = "";

		if (random(0,1) == 0) { // randomize order, this is annoyingly long but easy
			manipCheck_html += '<p class="block-text">How many of the ' + plural + ' have ' + 
			prop_words[target_prop] + '?' + '  <input type="text" id="manipCheckTarget" ' + 
			'name="manipCheckTarget" size="1"></p>';
			manipCheck_html += '<p class="block-text">How many of the ' + plural + ' have ' + 
			prop_words[distractor_prop] + '?' + 
			'  <input type="text" id="manipCheckDist" ' + 
			'name="manipCheckDist" size="1"></p>'	
		} else {
		    manipCheck_html += '<p class="block-text">How many of the ' + plural + ' have ' + 
			prop_words[distractor_prop] + '?' + 
			'  <input type="text" id="manipCheckDist" ' + 
			'name="manipCheckDist" size="1"></p>'	
		    manipCheck_html += '<p class="block-text">How many of the ' + plural + ' have ' + 
			prop_words[target_prop] + '?' + '  <input type="text" id="manipCheckTarget" ' + 
			'name="manipCheckTarget" size="1"></p>';
		} 

		$("#manipCheck").html(manipCheck_html)

		// CREATE CHOICE TEXT ETC
		if (word_cond == "listener") { // LISTENER CONDITION
			    var label_html = '<br><br><p class="block-text">Bob says: '
		    label_html += '<p class="block-text style="font-size:x-large;">' + 
			'"My favorite ' + base + ' has <b>' + 
			prop_words[target_prop] + 
			'."</b></p>';
		    label_html += '<p class="block-text">Click on the ' + base + 
			' that you think Bob is referring to.</p>';
		} else if (word_cond == "salience") { // SCHELLING (MUMBLE) CONDITION
			    var label_html = '<br><br><p class="block-text">Bob says: '
		    label_html += '<p class="block-text style="font-size:x-large;">' + 
			'"The ' + base + ' I most like to ' + actions[0] + 
			' has <b>mumblemumble."</b></p>' + 
			'<p class="block-text style="font-size:small;">' + 
			'(You couldn\'t hear what he said.)</p>';
		    label_html += '<p class="block-text">Click on the ' + base + 
			' that you think Bob is referring to.</p>';
		} else if (word_cond == "baserate") { // PURE BASE RATE CONDITION
			    var label_html = '<br><br><p class="block-text">Which ' + base + 
			' is Bob\'s favorite?</p><p class="block-text">Click on the ' + 
			base + ' you think is Bob\'s favorite.</p>';
		}

			$("#labelInst").html(label_html) 	
		},

		// SELECT FUNCTION (called in stage slide)
		select: function (c) {
		experiment.choice = choice_names[c];

		// unchoose everything
		for (var i=0; i<choices.length; i++) {
		    $("#tdchoice" + String(i)).removeClass('chosen').addClass('unchosen')
		}
		// choose this one
		$("#tdchoice" + String(c)).removeClass('unchosen').addClass('chosen')

		},

		// REVEAL IMAGES IN FAMILIARIZATION
		reveal: function(n) {

		day_html = stimHTML(base,fam_dist[fam_perm[n]],fam_mat[n],props,'obj')

		$("#day" + String(n)).html(day_html) 
		fam_clicked = unique(fam_clicked.concat(n))

		if (fam_clicked.length == num_fam) {
		    fam_finished = 1
		}
		},

		// CHECK THAT FAMILIARIZARION IS DONE
		check_fam: function() {
		if (fam_finished == 1) 
		{
		    famNextButton.blur(); 
		    experiment.next_test();

		} else {
		    $("#famMessage").html('<font color="red">' + 
				       'Please make sure you have looked at all the days!' + 
				       '</font>');
		}
    },

    // CHECK THAT TEST IS DONE
    check_test: function() {
	if (experiment.choice != "null" &&
	    document.getElementById("manipCheckTarget").value != "" &&
	    document.getElementById("manipCheckDist").value != "") 
	{
	    testNextButton.blur(); 
	    experiment.manip_check_target = document.getElementById("manipCheckTarget").value;
	    experiment.manip_check_dist = document.getElementById("manipCheckDist").value;

    	    showSlide("check");

	} else {
	    $("#testMessage").html('<font color="red">' + 
			       'Please make sure you have answered all the questions!' + 
			       '</font>');
	}
    },

   // FINISHED BUTTON CHECKS EVERYTHING AND THEN ENDS
    check_finished: function() {
	if (($("input[type=radio]:checked").length == 0) ||
	    document.getElementById('about').value.length < 1) {
	    $("#checkMessage").html('<font color="red">' + 
			       'Please make sure you have answered all the questions!' + 
			       '</font>');
	} else {
	    if ($("input[type=radio]:checked")[0].value) {
		experiment.name_check_correct = "TRUE";
	    }
	    experiment.about = document.getElementById("about").value;
	    experiment.comment = document.getElementById("comments").value;

    	    showSlide("finished");

	    if (experiment.choice == "target") {
		experiment.choice_correct = "TRUE";
	    } 
	    experiment.end();
	}
    },

    // END FUNCTION 
    end: function () {
        showSlide("finished");
        setTimeout(function () {

            // Decrement only if this is an actual turk worker!		
	    if (turk.workerId.length > 0){
		var xmlHttp = null;
		xmlHttp = new XMLHttpRequest();
		xmlHttp.open('GET',			 
			     'http://langcog.stanford.edu/cgi-bin/' + 
			     'subject_equalizer/decrementer.php?filename=' + 
			     filename + "&to_decrement=" + cond, false);
		xmlHttp.send(null);
	    }

            turk.submit(experiment);
        }, 500); 
    }
}