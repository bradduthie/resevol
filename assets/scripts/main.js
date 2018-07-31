// Javascript to handle data from the model and translate to animated page changes
// @author Rose McKeon
(function($){
	// when the page is fully loaded...
	$(document).ready(function(){
		/* 
		Get the data (output by R) formatted as:
		---
			"crops": [array or crop values for Y axis],
			"pathogens": [array of pathogens for X axis],
			"helicoverpa": [{crop, pathogen, genotype, resistance},{crop, pathogen, genotype, resistance}]
		---
		*/
		$.getJSON("./data/output.json", function(patches){
			if($.isEmptyObject(patches)) {
				// Handle missing data...
				console.log("No patch data");	
			} else {
				// If we have data Make the patches...
				//console.log(patches);
				var row = 'one-high';
				if(patches.crops.length == 2){
					row = 'two-high';
				} else if (patches.crops.length == 3){
					row = 'three-high';
				}
				var col = 'col-sm-12';
				if(patches.pathogens.length == 2){
					col = 'col-sm-6';
				} else if (patches.pathogens.length == 3){
					col = 'col-sm-4';
				}
				// make a row for every crop
				$.each(patches.crops, function(index, element){
					$('.patches').append('<div class="row crop-'+element+'"></div>');
				});
				// add a column for every pathogen
				$.each(patches.pathogens, function(index, element){
					$('.row').append('<div class="patch grid pathogen-'+element+' '+row+' '+col+'"></div>');
				});
				// fill each patch
				$.each(patches.helicoverpa, function(index, element){
					var size = element.resistance+'px';
					var icon = null;
					if(element.resistance <= 20){
						icon = 'fas fa-frown';
					} else if(element.resistance < 35){
						icon = 'fas fa-meh';
					} else {
						icon = 'fas fa-smile';
					}
					$('.crop-'+element.crop+' .pathogen-'+element.pathogen).append('<div class="helicoverpa grid-item genotype-'+element.genotype+' '+icon+'" style="font-size:'+size+';width: '+size+';height:'+size+';"></div>');
				});
				// arrange the individuals
				$('.grid').masonry({
				  // options...
				  itemSelector: '.grid-item',
				  columnWidth: 20,
				  horizontalOrder: false,
				  originTop: false,
				  //stagger: 30
				});
			}
		});
	}); // end document ready
})(jQuery); // Fully reference jQuery after this point.