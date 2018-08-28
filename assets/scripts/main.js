// Javascript to handle data from the model and translate to animated page changes
// @author Rose McKeon
(function($){
	// when the page is fully loaded...
	$(document).ready(function(){
		// make the sidebar work without redrawing missing helicoverpa
		$('#toggle-controls').click(function(){			
      $('.wrapper').toggleClass('open-controls');
    });
		// default input (0 crops, 0 pathogens)
		//var crops = 0;
		//var pathogens = 0;
		//var input = $.parseJSON( '{"crops": '+crops+', "pathogens": '+pathogens+'}' );
		//console.log(input);
		// Watch the sliders - submitted now via GET as easier to get working wth rApache
		/*
		$("#crops, #pathogens").on("change", function(slideEvt) {
			if(slideEvt.currentTarget.id == "crops"){
				// update crops if change detected
				crops = slideEvt.value.newValue;
			}else if(slideEvt.currentTarget.id == "pathogens"){
				// update pathogens if change detected
				pathogens = slideEvt.value.newValue;
			}
			// parse updated JSON to send to R function
			// input = $.parseJSON( '{"crops": '+crops+', "pathogens": '+pathogens+'}' );
			// set cookies with form data to access from R
			$.cookie('crops', crops);
			$.cookie('pathogens', pathogens);
			console.log($.cookie('crops'));
			console.log($.cookie('pathogens'));
		});
		*/
		// Get the data (output by R):
		
		// take the json output from toy model and build html landscape
		
		/*
		$.getJSON('./data/sample_sim2.json').done(function(patches) {
	    if($.isEmptyObject(patches)) {
				// Handle missing data...
				console.log("No patch data");	
				// make the sidebar work without redrawing missing helicoverpa
				$('#toggle-controls').click(function(){			
		      $('.wrapper').toggleClass('open-controls');
		    });
			} else {
				// If we have data label the patches...
				//console.log(patches);
				$.each(patches.crop_val.crop_val, function(index, element){
					element = element[0]; 
					$('.y-'+element.yloc+' .x-'+element.xloc).addClass('crop-'+element.val);
				});
				$.each(patches.path_val.path_val, function(index, element){
					element = element[0]; 
					$('.y-'+element.yloc+' .x-'+element.xloc).addClass('pathogen-'+element.val);
				});
				// fill each patch
				$.each(patches.values.values, function(index, element){
					element = element[0];
					// generate colours based on genotype
					var R = element.c_geno * 4;
					var G = element.p_geno * 4;
					var B = element.c_geno * 3 + element.p_geno * 3; 
					var rgb = "rgb("+R+","+G+","+B+")";
					// set icon based on resulting traits
					var icon = null;
					if(!element.eat_crop && !element.resist_path){
						// can neither resist pathogen or eat crop
						icon = 'fas fa-frown';
					} else if(element.eat_crop && element.resist_path){
						// can eat crop and resist pathogen
						icon = 'fas fa-smile';
					} else {
						// can eat crop or resist pathogen, but can't do both
						icon = 'fas fa-meh';
					}
					$('.y-'+element.yloc+' .x-'+element.xloc).append('<div class="helicoverpa grid-item c-'+element.c_geno+' p-'+element.p_geno+' '+icon+'" style="color: '+rgb+'"></div>');
				});
				// arrange helicoverpa
				var $grid = $('.grid').masonry({
				  itemSelector: '.grid-item',
				  columnWidth: 20,
				  horizontalOrder: false,
				  originTop: false,
				  isInitLayout: false,
				  //resize: false
				  //stagger: 30
				});
				$grid.masonry();
				// the sidebar
				$('#toggle-controls').click(function(){			
		      $('.wrapper').toggleClass('open-controls');
		      $('.wrapper #content').one('webkitTransitionEnd otransitionend oTransitionEnd msTransitionEnd transitionend', function(e) {
				    // rearrarange helicoverpa when the transistion ends
						$grid.masonry();
				  });
		    });
			}			
	  })
	  .fail(function() {
	    // Handle missing data...
			console.log("No output file");
			// make the sidebar work without redrawing missing helicoverpa
			$('#toggle-controls').click(function(){			
	      $('.wrapper').toggleClass('open-controls');
	    });
	  });	
	  */
	}); // end document ready
})(jQuery); // Fully reference jQuery after this point.

function buildLandscape(patches){
	if(typeof patches == 'object'){
		console.log(patches);
		if($.isEmptyObject(patches)) {
				// Handle missing data...
				console.log("No patch data");	
				
			} else {
				// If we have data label the patches...
				//console.log(patches);
				$.each(patches.values.cell_vals, function(index, element){
					element = element[0]; 
					$('.y-'+element.yloc+' .x-'+element.xloc)
						.addClass('crop-'+element.crop).addClass('pathogen-'+element.pathogen);
				});

				// fill each patch
				$.each(patches.values.values, function(index, element){
					element = element[0];
					// generate colours based on genotype
					var R = element.c_geno * 4;
					var G = element.p_geno * 4;
					var B = element.c_geno * 3 + element.p_geno * 3; 
					var rgb = "rgb("+R+","+G+","+B+")";
					// set icon based on resulting traits
					var icon = null;
					if(!element.eat_crop && !element.resist_path){
						// can neither resist pathogen or eat crop
						icon = 'fas fa-frown';
					} else if(element.eat_crop && element.resist_path){
						// can eat crop and resist pathogen
						icon = 'fas fa-smile';
					} else {
						// can eat crop or resist pathogen, but can't do both
						icon = 'fas fa-meh';
					}
					$('.y-'+element.yloc+' .x-'+element.xloc).append('<div class="helicoverpa grid-item c-'+element.c_geno+' p-'+element.p_geno+' '+icon+'" style="color: '+rgb+'"></div>');
				});
				// arrange helicoverpa
				var $grid = $('.grid').masonry({
				  itemSelector: '.grid-item',
				  columnWidth: 20,
				  horizontalOrder: false,
				  originTop: false,
				  isInitLayout: false,
				  //resize: false
				  //stagger: 30
				});
				$grid.masonry();
				// the sidebar
				$('#toggle-controls').click(function(){			
		      $('.wrapper').toggleClass('open-controls');
		      $('.wrapper #content').one('webkitTransitionEnd otransitionend oTransitionEnd msTransitionEnd transitionend', function(e) {
				    // rearrarange helicoverpa when the transistion ends
						$grid.masonry();
				  });
		    });
			}			
	} else {
		console.log('landscape data not in JSON format');
	}
}
	