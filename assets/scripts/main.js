// Javascript to handle data from the model and translate to animated page changes
// @author Rose McKeon
(function($){
	// when the page is fully loaded...
	$(document).ready(function(){
		// make the sidebar work without redrawing missing helicoverpa
		$('#toggle-controls').click(function(){			
      $('.wrapper').toggleClass('open-controls');
    });
	}); // end document ready
})(jQuery); // Fully reference jQuery after this point.

function buildSummary(summary, labels){
	if($.isArray(summary)){
		// build summary
		var title = '<h4>Summary</h4>';
		var list = '<ul class="list-group">';
		$.each(summary, function(index, element){
			var colourclass = '';
			if(index == 0){ 
				// Population size
				if(element <= 200){
					colourclass = " list-group-item-success";
				}
				if(element < 400 & element > 200 ){
					colourclass = " list-group-item-warning";
				}
				if(element > 399){
					colourclass = " list-group-item-danger";
				}
			}
			if(index >= 3){ 
				// Percentage resistant / crop-eaters
				if(element > 50){
					colourclass = " list-group-item-warning";
				}
				if(element > 75){
					colourclass = " list-group-item-danger";
				}
				if(element <= 50){
					colourclass = " list-group-item-success";
				}
				element = element+' %';
			}
			list += '<li class="list-group-item'+colourclass+'"><span class="summary-label">'+labels[index]+'</span><strong class="summary-value">'+element+'</strong></li>';
		});
		list += '</ul>';
		$('.summary').append(title+list);
	} else {
		// handle error
		console.log('summary data is not an array');
	}
}

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
		    // build the summary pane
		    buildSummary(patches.values.land_vals, ['Population size', 'Resistance genotypes', 'Crop-eating genotypes', 'Percentage resistant', 'Percentage crop-eaters']);
			}			
	} else {
		console.log('landscape data not in JSON format');
	}
}
	