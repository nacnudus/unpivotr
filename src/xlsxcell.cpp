#include <Rcpp.h>
#include "rapidxml.h"
#include "xlsxbook.h"
#include "xlsxcell.h"
#include "utils.h"

using namespace Rcpp;

xlsxcell::xlsxcell(rapidxml::xml_node<>* c,
    double& height, std::vector<double>& colWidths, xlsxbook& book): 
  c_(c), height_(height), book_(book) {
    rapidxml::xml_attribute<>* r = c_->first_attribute("r");
    if (r == NULL)
      stop("Invalid cell: lacks 'r' attribute");
    address_ = std::string(r->value());
    parseAddress(address_, row_, col_);
    width_ = colWidths[col_ - 1];

    getChildValueString(content_, "v", c_);
    getChildValueString(formula_, "f", c_);
    getAttributeValueString(type_, "t", c_);

    /* rapidxml::xml_attribute<>* type = cell_->first_attribute("t"); */
    /* if (type == NULL) { */
    /*   has_type_ = false; */
    /* } else { */
    /*   has_type_ = true; */
    /*   type_ = std::string(type->value()); */
    /* } */

    /* rapidxml::xml_node<>* formula = cell_->first_node("f"); */
    /* if (formula == NULL) { */
    /*   has_formula_ = false; */
    /* } else { */
    /*   has_formula_ = true; */
    /*   formula_ = std::string(formula->value()); */
    /* } */

    /* rapidxml::xml_attribute<>* style = cell_->first_attribute("s"); */
    /* if (style == NULL) {style_ = 0;} else {style_ = atoi(style->value());} */

}

std::string xlsxcell::address() {return(address_);}
int xlsxcell::row() {return(row_);}
int xlsxcell::col() {return(col_);}
String xlsxcell::content() {return content_;}
String xlsxcell::formula() {return formula_;}
String xlsxcell::type() {return type_;}
double xlsxcell::height() {return height_;}
double xlsxcell::width() {return width_;}
