package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"sort"
	"strconv"
	"strings"

	"github.com/paulmach/orb"
	"github.com/paulmach/orb/planar"
)

type Point struct {
	x int
	y int
}

type Rectangle struct {
	area       int
	minX, minY int
	maxX, maxY int
}

func parseInput() []Point {

	// Method 1: Read file line by line using bufio.Scanner
	file, err := os.Open("day09_input.txt")
	if err != nil {
		log.Fatal(err)
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)

	lines := make([]string, 0)
	for scanner.Scan() {
		lines = append(lines, scanner.Text())
	}

	if err := scanner.Err(); err != nil {
		log.Fatal(err)
	}

	points := make([]Point, 0)
	for _, line := range lines {
		d := strings.Split(line, ",")

		x, _ := strconv.Atoi(d[0])
		y, _ := strconv.Atoi(d[1])

		points = append(points, Point{x: x, y: y})
	}

	return points
}

func toEdges(points []Point) [][2]Point {
	points = append(points, points[0])
	edges := make([][2]Point, 0)
	for i := 1; i < len(points); i++ {
		edges = append(edges, [2]Point{points[i-1], points[i]})
	}

	return edges
}

// Alternative: Create orb.Polygon from points directly
func pointsToOrbPolygon(points []Point) orb.Polygon {
	ring := make(orb.Ring, len(points)+1)

	for i, p := range points {
		ring[i] = orb.Point{float64(p.x), float64(p.y)}
	}

	// Close the ring
	ring[len(points)] = ring[0]

	return orb.Polygon{ring}
}

func toRectangles(points []Point) []Rectangle {
	recs := make([]Rectangle, 0)
	for i := 0; i < len(points); i++ {
		for j := i + 1; j < len(points); j++ {
			r := Rectangle{
				minX: min(points[i].x, points[j].x),
				minY: min(points[i].y, points[j].y),
				maxX: max(points[i].x, points[j].x),
				maxY: max(points[i].y, points[j].y),
			}
			r.area = ((r.maxX - r.minX) + 1) * ((r.maxY - r.minY) + 1)
			recs = append(recs, r)
		}
	}

	sort.Slice(recs, func(i int, j int) bool {
		return recs[i].area > recs[j].area
	})
	return recs
}

func main() {
	points := parseInput()
	edges := toEdges(points)
	rectangles := toRectangles(points)

	// Create orb.Polygon from edges
	polygon := pointsToOrbPolygon(points)

	// Alternative: create directly from points
	// polygon := pointsToOrbPolygon(points)

	for _, rec := range rectangles {
		// fmt.Println("checking", rec)

		p1 := orb.Point{float64(rec.minX) + 0.1, float64(rec.minY) + 0.1}
		p2 := orb.Point{float64(rec.minX) + 0.1, float64(rec.maxY) - 0.1}
		p3 := orb.Point{float64(rec.maxX) - 0.1, float64(rec.minY) + 0.1}
		p4 := orb.Point{float64(rec.maxX) - 0.1, float64(rec.maxY) - 0.1}

		ps := []orb.Point{p1, p2, p3, p4}

		ok := true
		for _, p := range ps {
			ok = ok && planar.PolygonContains(polygon, p)
		}

		for _, e := range edges {
			ep1 := orb.Point{float64(e[0].x), float64(e[0].y)}
			ep2 := orb.Point{float64(e[1].x), float64(e[1].y)}

			// Check if edge intersects with any rectangle edge
			if intersect(p1, p2, ep1, ep2) ||
				intersect(p2, p4, ep1, ep2) ||
				intersect(p4, p3, ep1, ep2) ||
				intersect(p3, p1, ep1, ep2) {
				ok = false
			}
		}

		if ok {
			fmt.Println("found:", rec)
			// break
		}
	}
}

func ccw(A, B, C orb.Point) bool {
	return (C[1]-A[1])*(B[0]-A[0]) > (B[1]-A[1])*(C[0]-A[0])
}

func intersect(A, B, C, D orb.Point) bool {
	return ccw(A, C, D) != ccw(B, C, D) && ccw(A, B, C) != ccw(A, B, D)
}
