/* eslint-disable */

/** @typedef {{x:number,y:number}} */
var Point;

var greinerHormann = {
   /**
     * @param  {Array<Point>} polygonA
     * @param  {Array<Point>} polygonB
     * @param  {Boolean=}               sourceForwards
     * @param  {Boolean=}               clipForwards
     * @return {?Array<Array<Point>>}
     */
    clip: function(polygonA, polygonB, sourceForwards, clipForwards) {},

    /**
     * @param  {Array<Point>} polygonA
     * @param  {Array<Point>} polygonB
     * @return {?Array<Array<Point>>}
     */
    diff: function(polygonA, polygonB) {},

    /**
     * @param  {Array<Point>} polygonA
     * @param  {Array<Point>} polygonB
     * @return {?Array<Array<Point>>}
     */
    union: function(polygonA, polygonB) {},

    /**
     * @param  {Array<Point>} polygonA
     * @param  {Array<Point>} polygonB
     * @return {?Array<Array<Point>>}
     */
    intersection: function(polygonA, polygonB) {}

};
